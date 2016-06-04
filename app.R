library(shiny)

xidefault <- c(-0.1, 0.05, 0.03, -0.2)
etadefault <- c(-0.2,0.5)
Adefault <- c(0.5, 0.05)

ui <- fluidPage(
  headerPanel("Food Demand Model"),
  sidebarPanel( width=3,
    h2("Model Parameters"),
    fluidRow(
      column(width=4, withMathJax(h3("\\(\\xi\\) values")))
    ),
    fluidRow(
      column(width=4, numericInput(inputId="xiss", value=xidefault[1],label="")),
      column(width=4, numericInput(inputId="xisn", value=xidefault[2], label=""))
      ),
    fluidRow(
      column(width=4, numericInput(inputId="xins", value=xidefault[3], label="")),
      column(width=4, numericInput(inputId="xinn", value=xidefault[4], label=""))
    ),
    h3("\\(\\eta\\) values"),
    fluidRow(
      column(width=4,
        numericInput(inputId="etas", value=etadefault[1],label=""),
        numericInput(inputId="etan", value=etadefault[2],label="")
      )
    ),
    h3('A values'),
    fluidRow(
      column(width=4,
        numericInput(inputId="As", value=Adefault[1],label=""),
        numericInput(inputId="An", value=Adefault[2],label="")
      )
    )    
  ),
  mainPanel(
  h2('Model Output'),
  dataTableOutput('Output Values')
  )
)

server <- function(input, output) {}

shinyApp(ui = ui, server = server)

