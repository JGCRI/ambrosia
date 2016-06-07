## Layout functions for the UI.

xidefault <- c(-0.1, 0.05, 0.03, -0.2)
elasmin <- -2
elasmax <- 2
elasstep <- 0.01
etastep <- 0.05

xi.matrix.input <- function()
{
  ## Draw the xi input table
  tags$table(
    tags$tr(
      tags$th(' '), tags$th('S',style='text-align: center'), tags$th('N',style='text-align: center')
    ),
    tags$tr(
      tags$th('S', HTML('&nbsp;&nbsp;')),   # hack until we get around to adding some style params for tables
      tags$td(numericInput(inputId="xiss", value=xidefault[1],label="",
                                         min=elasmin, max=elasmax, step=elasstep)),
      tags$td(numericInput(inputId="xisn", value=xidefault[2], label="",
                           min=elasmin, max=elasmax, step=elasstep))
    ),
    tags$tr(
      tags$th('N'),
      tags$td(numericInput(inputId="xins", value=xidefault[3], label="",
                           min=elasmin, max=elasmax, step=elasstep)),
      tags$td(numericInput(inputId="xinn", value=xidefault[4], label="",
                           min=elasmin, max=elasmax, step=elasstep))
    )
  )
}


column.input.table <- function(inputids, defvals, min, max, step)
{
  ## Draw an input table with a single column of two values
  ##  inputids and defvals are vectors
  ##  min, max, and step are single values.
  tags$table(
    tags$tr(
      tags$th('S', HTML('&nbsp;&nbsp;')),
      tags$td(numericInput(inputId=inputids[1], value=defvals[1], label='',
                           min=min, max=max, step=step))
    ),
    tags$tr(
      tags$th('N'),
      tags$td(numericInput(inputId=inputids[2], value=defvals[2], label='',
                           min=min, max=max, step=step))
    )
  )
}