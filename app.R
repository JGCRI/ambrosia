library(shiny)
library(ggplot2)

xidefault <- c(-0.1, 0.05, 0.03, -0.2)
etadefault <- c(-0.2,0.5)
Adefault <- c(0.5, 0.05)

elasmin <- -2
elasmax <- 2
elasstep <- 0.01
etastep <- 0.05

ui <- fluidPage(
  headerPanel(h1("Food Demand Model",align='center'),windowTitle='Food Demand Model'),
  sidebarLayout(
  sidebarPanel( width=3,
    h2("Model Parameters"),
    fluidRow(
      column(width=8, withMathJax(h3("\\(\\xi\\) values")))
    ),
    fluidRow(
      column(width=4, numericInput(inputId="xiss", value=xidefault[1],label="",
                                   min=elasmin, max=elasmax, step=elasstep)),
      column(width=4, numericInput(inputId="xisn", value=xidefault[2], label="",
                                   min=elasmin, max=elasmax, step=elasstep))
      ),
    fluidRow(
      column(width=4, numericInput(inputId="xins", value=xidefault[3], label="",
                                   min=elasmin, max=elasmax, step=elasstep)),
      column(width=4, numericInput(inputId="xinn", value=xidefault[4], label="",
                                   min=elasmin, max=elasmax, step=elasstep))
    ),
    h3("\\(\\eta\\) values"),
    fluidRow(
      column(width=4,
        numericInput(inputId="etas", value=etadefault[1],label="", 
                     min=elasmin, max=elasmax, step=etastep),
        numericInput(inputId="etan", value=etadefault[2],label="", 
                     min=elasmin, max=elasmax, step=etastep)
      )
    ),
    h3('A values'),
    fluidRow(
      column(width=4,
        numericInput(inputId="As", value=Adefault[1],label="", min=0, max=1, step=0.05),
        numericInput(inputId="An", value=Adefault[2],label="", min=0, max=1, step=0.05)
      )
    )    
  ),
  
  ## Main Panel
  mainPanel(
    h2('Model Output'),
    tabsetPanel(
      tabPanel(title="By pcGDP",
               tableOutput(outputId='output.Y'),
               h3('Demand by Income',align='center'),
               plotOutput(outputId='plot.Q.Y')),
      tabPanel(title="By \\(P_s\\)",
               tableOutput(outputId='output.Ps'),
               h3('Demand by Staple Price',align='center'),
               plotOutput(outputId='plot.Q.Ps')),
      tabPanel(title="By \\(P_n\\)",
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
  list(
    xi=matrix(c(input$xiss, input$xins, input$xisn, input$xinn), nrow=2),
    yfunc=c(eta.constant(input$etas), eta.constant(input$etan)),
    A=c(input$As, input$An))
}

server <- function(input, output) {
  model.data <- reactive({
    ## Compute results for income change
    params <- set.model.params(input)
    rslt <- food.dmnd(1,1,y.vals,params)
    ydata <- data.frame(Ps=1, Pn=1, Y=y.vals, alpha.s=rslt$alpha.s, alpha.n=rslt$alpha.n,
                        Qs=rslt$Qs, Qn=rslt$Qn)
    
    ## compute results for staple price change
    yvals <- rep(1,length(Ps.vals))
    rslt <- food.dmnd(Ps.vals, 1, yvals, params)
    cond.1 <- rslt$alpha.s*params$yfunc[[1]](yvals)
    cond.2 <- rslt$alpha.n*params$yfunc[[2]](yvals)
    cond.3 <- cond.1+cond.2
    psdata <- data.frame(Ps=Ps.vals, Pn=1, Y=1, alpha.s=rslt$alpha.s, alpha.n=rslt$alpha.n,
                         Qs=rslt$Qs, Qn=rslt$Qn,
                         `alpha.s*eta.s`=cond.1,
                         `alpha.n*eta.n`=cond.2,
                         `sum(alpha.i*eta.i)`=cond.3)
    
    ## compute results for nonstaple price change
    yvals <- rep(1,length(Pn.vals))
    rslt <- food.dmnd(1, Pn.vals, yvals, params)
    cond.1 <- rslt$alpha.s*params$yfunc[[1]](yvals)
    cond.2 <- rslt$alpha.n*params$yfunc[[2]](yvals)
    cond.3 <- cond.1+cond.2
    pndata <- data.frame(Ps=1, Pn=Pn.vals, Y=1, alpha.s=rslt$alpha.s, alpha.n=rslt$alpha.n,
                         Qs=rslt$Qs, Qn=rslt$Qn, 
                         `alpha.s*eta.s`=cond.1,
                         `alpha.n*eta.n`=cond.2,
                         `sum(alpha.i*eta.i)`=cond.3)
    
    ## return all of the above
    list(ydata=ydata, psdata=psdata, pndata=pndata)
  })
  
  output$output.Y <- renderTable({
    model.data()$ydata
  })
  output$output.Ps <- renderTable({
    model.data()$psdata
  })
  output$output.Pn <- renderTable({
    model.data()$pndata
  })
  output$plot.Q.Ps <- renderPlot({
    make.demand.plot(model.data()$psdata,Ps.vals,'Price (staples)')
  })
  output$plot.Q.Pn <- renderPlot({
    make.demand.plot(model.data()$pndata,Pn.vals,'Price (nonstaples)')
  })
  output$plot.Q.Y <- renderPlot({
    make.demand.plot(model.data()$ydata,y.vals,'per-capita Income')
  })
}

shinyApp(ui = ui, server = server)

