library(shiny)

xidefault <- c(-0.1, 0.05, 0.03, -0.2)
etadefault <- c(-0.2,0.5)
Adefault <- c(0.5, 0.05)

elasmin <- -2
elasmax <- 2
elasstep <- 0.01
etastep <- 0.05

ui <- fluidPage(
  headerPanel("Food Demand Model"),
  sidebarPanel( width=3,
    h2("Model Parameters"),
    fluidRow(
      column(width=4, withMathJax(h3("\\(\\xi\\) values")))
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
               tableOutput(outputId='output.Y')),
      tabPanel(title="By \\(P_s\\)",
               tableOutput(outputId='output.Ps')),
      tabPanel(title="By \\(P_n\\)",
               tableOutput(outputId='output.Pn'))
    )
  )
)

source('food-demand.R')

set.model.params <-function(input)
{
  list(
    xi=matrix(c(input$xiss, input$xins, input$xisn, input$xinn), nrow=2),
    yfunc=c(eta.constant(input$etas), eta.constant(input$etan)),
    A=c(input$As, input$An))
}

server <- function(input, output) {
  output$output.Y <- renderTable({
    params <- set.model.params(input)
    rslt <- food.dmnd(1,1,y.vals,params)
    data.frame(Ps=1, Pn=1, Y=y.vals, alpha.s=rslt$alpha.s, alpha.n=rslt$alpha.n,
               Qs=rslt$Qs, Qn=rslt$Qn)
  })
  output$output.Ps <- renderTable({
    params <- set.model.params(input)
    yvals <- rep(1,length(Ps.vals))
    rslt <- food.dmnd(Ps.vals, 1, yvals, params)
    cond.1 <- rslt$alpha.s*params$yfunc[[1]](yvals)
    cond.2 <- rslt$alpha.n*params$yfunc[[2]](yvals)
    cond.3 <- cond.1+cond.2
    data.frame(Ps=Ps.vals, Pn=1, Y=1, alpha.s=rslt$alpha.s, alpha.n=rslt$alpha.n,
               Qs=rslt$Qs, Qn=rslt$Qn,
               `alpha.s*eta.s`=cond.1,
               `alpha.n*eta.n`=cond.2,
               `sum(alpha.i*eta.i)`=cond.3)
  })
  output$output.Pn <- renderTable({
    params <- set.model.params(input)
    yvals <- rep(1,length(Pn.vals))
    rslt <- food.dmnd(1, Pn.vals, yvals, params)
    cond.1 <- rslt$alpha.s*params$yfunc[[1]](yvals)
    cond.2 <- rslt$alpha.n*params$yfunc[[2]](yvals)
    cond.3 <- cond.1+cond.2
    data.frame(Ps=1, Pn=Pn.vals, Y=1, alpha.s=rslt$alpha.s, alpha.n=rslt$alpha.n,
               Qs=rslt$Qs, Qn=rslt$Qn, 
               `alpha.s*eta.s`=cond.1,
               `alpha.n*eta.n`=cond.2,
               `sum(alpha.i*eta.i)`=cond.3)
  })
}

shinyApp(ui = ui, server = server)

