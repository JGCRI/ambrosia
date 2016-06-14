library(shiny)
library(ggplot2)

etadefault <- c(-0.1,0.8)
Adefault <- c(0.5, 0.2)

source('ui-fcns.R')

ui <- fluidPage(
  headerPanel(h1("Food Demand Model",align='center'),windowTitle='Food Demand Model'),
    sidebarLayout(
        sidebarPanel(
            fluidRow(h2("Model Parameters")),
            fluidRow(
                column(width=8, withMathJax(h3("Price elasticity model")))
                ),
            fluidRow(
                xi.matrix.input()
                ),
            fluidRow(h3("Income elasticity model")),
            fluidRow(column(8,h4("Staple demand"))),
            fluidRow(column(8,eta.selector('eta.s.select','\\(\\eta = f_s(Y)\\)',1))),
            fluidRow(
                column(4,
                       numericInput(inputId='etas', value=etadefault[1], label='elasticity (Y=1)',
                                    min=elasmin, max=elasmax, step=etastep)),
                column(4, 
                       conditionalPanel('input["eta.s.select"] == 2',
                                        numericInput(inputId='y0val', label='Y\\(_0\\)',
                                                     value=0.5, min=0.1, max=10, step=0.1)))),
            fluidRow(column(8,h4('Non-staple demand'))),
            fluidRow(column(8,eta.selector('eta.n.select', '\\(\\eta = f_n(Y)\\)' ,2))),
            fluidRow(column(4,
                            numericInput(inputId='etan', value=etadefault[2], label='elasticity (Y=1)',
                                         min=elasmin, max=elasmax, step=etastep))),
            
            fluidRow(h3('Q values (\\(Y=1\\))')),
            fluidRow(
                column(1,
                       column.input.table(c('As','An'), Adefault, 0, 1, 0.05))
                ),
            fluidRow(h3('Other Price and Income Variables')),
            conditionalPanel(condition='input.tab != 1',
                             sliderInput(inputId='y.val.slider', min=0, max=50.0, step=0.5, label='\\(Y\\)',
                                         value=1)),
            conditionalPanel(condition='input.tab != 2',
                             sliderInput(inputId='ps.val.slider', min=0.1, max=20.0, step=0.1, label='\\(P_s\\)',
                                         value=1)),
            conditionalPanel(condition='input.tab != 3',
                             sliderInput(inputId='pn.val.slider', min=0.1, max=20.0, step=0.1, label='\\(P_n\\)',
                                         value=1))
            ),
  
  ## Main Panel
  mainPanel(
    h2('Model Output'),
    tabsetPanel(id="tab",
      tabPanel(title="By pcGDP", value=1,
               tableOutput(outputId='output.Y'),
               h3('Demand by Income',align='center'),
               plotOutput(outputId='plot.Q.Y')),
      tabPanel(title="By \\(P_s\\)", value=2,
               tableOutput(outputId='output.Ps'),
               h3('Demand by Staple Price',align='center'),
               plotOutput(outputId='plot.Q.Ps')),
      tabPanel(title="By \\(P_n\\)", value=3,
               tableOutput(outputId='output.Pn'),
               h3('Demand by Nonstaple Price', align='center'),
               plotOutput(outputId='plot.Q.Pn'))
    )
  )
  )  # end of sidebar layout
)


source('food-demand.R')
source('food-demand-plots.R')

set.model.params <-function(input)
{
  if(input$eta.s.select == 1) {
      eta.s.fn <- eta.constant(input$etas)
          ##c(eta.constant(input$etas), eta.constant(input$etan))
  }
  else {
      eta.s.fn <- eta.s(input$etas, input$y0val)
          ##c(eta.s(input$etas, input$y0val), eta.n(input$etan))
  }

  if(input$eta.n.select == 1) {
      eta.n.fn <- eta.constant(input$etan)
  }
  else {
      eta.n.fn <- eta.n(input$etan)
  }
  eta.fns <- c(eta.s.fn, eta.n.fn)
  
  list(
    xi=matrix(c(input$xiss, input$xins, input$xisn, input$xinn), nrow=2),
    yfunc=eta.fns,
    A=c(input$As, input$An))
}

## data frames to hold persistent results.
ydata  <- data.frame(Ps=1, Pn=1, Y=1, alpha.s=0, alpha.n=0,Qs=0,Qn=0)
psdata <- data.frame(Ps=1, Pn=1, Y=1, alpha.s=0, alpha.n=0,Qs=0,Qn=0)
pndata <- data.frame(Ps=1, Pn=1, Y=1, alpha.s=0, alpha.n=0,Qs=0,Qn=0)

server <- function(input, output) {
  model.data <- reactive({
    ## Compute results for income change
    params <- set.model.params(input)
    if(input$tab == 1) {
      ps <- rep(input$ps.val.slider, length(y.vals))
      pn <- rep(input$pn.val.slider, length(y.vals))
      rslt <- food.dmnd(ps,pn,y.vals,params)
      ydata <<- data.frame(Ps=ps, Pn=pn, Y=y.vals, alpha.s=rslt$alpha.s, alpha.n=rslt$alpha.n,
                          Qs=rslt$Qs, Qn=rslt$Qn)
    }
    
    ## compute results for staple price change
    if(input$tab == 2) {
      yvals <- rep(input$y.val.slider,length(Ps.vals))
      pn <- rep(input$pn.val.slider, length(Ps.vals))
      rslt <- food.dmnd(Ps.vals, pn, yvals, params)
      cond.1 <- rslt$alpha.s*params$yfunc[[1]](yvals)
      cond.2 <- rslt$alpha.n*params$yfunc[[2]](yvals)
      cond.3 <- cond.1+cond.2
      psdata <<- data.frame(Ps=Ps.vals, Pn=pn, Y=yvals, alpha.s=rslt$alpha.s, alpha.n=rslt$alpha.n,
                           Qs=rslt$Qs, Qn=rslt$Qn,
                           `alpha.s*eta.s`=cond.1,
                           `alpha.n*eta.n`=cond.2,
                           `sum(alpha.i*eta.i)`=cond.3)
    }
    
    ## compute results for nonstaple price change
    if(input$tab == 3) {
      yvals <- rep(input$y.val.slider,length(Pn.vals))
      ps <- rep(input$ps.val.slider, length(Pn.vals))
      rslt <- food.dmnd(ps, Pn.vals, yvals, params)
      cond.1 <- rslt$alpha.s*params$yfunc[[1]](yvals)
      cond.2 <- rslt$alpha.n*params$yfunc[[2]](yvals)
      cond.3 <- cond.1+cond.2
      pndata <<- data.frame(Ps=ps, Pn=Pn.vals, Y=yvals, alpha.s=rslt$alpha.s, alpha.n=rslt$alpha.n,
                           Qs=rslt$Qs, Qn=rslt$Qn, 
                           `alpha.s*eta.s`=cond.1,
                           `alpha.n*eta.n`=cond.2,
                           `sum(alpha.i*eta.i)`=cond.3)
    }
    ## return all of the above
    list(ydata=ydata, psdata=psdata, pndata=pndata)
  })
  
    output$output.Y <- renderTable({
      model.data()$ydata
    })
    output$plot.Q.Y <- renderPlot({
      make.demand.plot(model.data()$ydata,y.vals,'per-capita Income')
    })

    output$output.Ps <- renderTable({
      model.data()$psdata
    })
    output$plot.Q.Ps <- renderPlot({
      make.demand.plot(model.data()$psdata,Ps.vals,'Price (staples)')
    })

    output$output.Pn <- renderTable({
      model.data()$pndata
    })
    output$plot.Q.Pn <- renderPlot({
      make.demand.plot(model.data()$pndata,Pn.vals,'Price (nonstaples)')
    })
}

shinyApp(ui = ui, server = server)

