library(shiny)

xidefault <- c(-0.1, 0.05, 0.03, -0.2)
etadefault <- c(-0.2,0.5)
Adefault <- c(0.5, 0.05)

elasmin <- -2
elasmax <- 2
elasstep <- 0.1

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
                     min=elasmin, max=elasmax, step=elasstep),
        numericInput(inputId="etan", value=etadefault[2],label="",
                     min=elasmin, max=elasmax, step=elasstep)
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
  mainPanel(
  h2('Model Output'),
  tableOutput(outputId='output.values')
  )
)

source('food-demand.R')

server <- function(input, output) {
  output$output.values <- renderTable({
    params <- list(
      xi=matrix(c(input$xiss, input$xins, input$xisn, input$xinn), nrow=2),
      yfunc=c(eta.constant(input$etas), eta.constant(input$etan)),
      A=c(input$As, input$An))
    rslt <- food.dmnd(1,1,y.vals,params)
    data.frame(Ps=1, Pn=1, Y=y.vals, alpha.s=rslt$alpha.s, alpha.n=rslt$alpha.n,
               Qs=rslt$Qs, Qn=rslt$Qn)
  })
}

shinyApp(ui = ui, server = server)

