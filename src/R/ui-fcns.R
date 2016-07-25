## Layout functions for the UI.

xidefault <- c(-0.03, 0.01, 0.05, -0.4)
elasmin <- -2
elasmax <- 2
elasstep <- 0.01
etastep <- 0.05
spacer <- HTML(paste0(rep('&nbsp;',5),collapse=''))

## A possibly-interesting parameter set (for the variable exponent model):
## xi <- c(-0.1, 0.05, 0.01, -0.5)
## eta <- c(-0.1, 1.5)
## y0 <- 0.5
## Q <- c(0.4,0.2)

## Possibly interisting parameter set for the constant exponent model:
## xi <- c(-0.15, 0.1, 0.1, -0.6)
## eta <- c(-0.3, 0.3)
## Q <- c(0.55, 0.05)

xi.matrix.input <- function()
{
  ## Draw the xi input table
  tags$table(
    tags$tr(
      tags$th(spacer),
      tags$td(numericInput(inputId="xiss", value=xidefault[1],label="\\(\\xi_{ss}\\)",
                           min=elasmin, max=elasmax, step=elasstep, width='75px')),
      tags$td(numericInput(inputId="xisn", value=xidefault[3], label="\\(\\xi_{sn}\\)",
                           min=elasmin, max=elasmax, step=elasstep, width='75px'))
    ),
    tags$tr(
      tags$th(spacer),
      tags$td(numericInput(inputId="xins", value=xidefault[2], label="\\(\\xi_{ns}\\)",
                           min=elasmin, max=elasmax, step=elasstep, width='75px')),
      tags$td(numericInput(inputId="xinn", value=xidefault[4], label="\\(\\xi_{nn}\\)",
                           min=elasmin, max=elasmax, step=elasstep, width='75px'))
    )
  )
}


y0.input.box <- function()
{
  tags$table(
    tags$tr(
      tags$td(
        numericInput(inputId='y0val', label='Y\\(_0\\)',width='75px',
               0.5, 0.1, 10, 0.1))))
}

column.input.table <- function(inputids, defvals, min, max, step, labels=c('Staple','Nonstaple'))
{
  ## Draw an input table with a single column of two values
  ##  inputids and defvals are vectors
  ##  min, max, and step are single values.
  tags$table(
    tags$tr(
      tags$th(spacer),
      tags$td(numericInput(inputId=inputids[1], value=defvals[1], label=labels[1],
                           min=min, max=max, step=step))
    ),
    tags$tr(
      tags$th(spacer),
      tags$td(numericInput(inputId=inputids[2], value=defvals[2], label=labels[2],
                           min=min, max=max, step=step))
    )
  )
}

eta.selector <- function(id,label2='\\(\\eta=f(Y)\\)',sel=1)
{
  eta.choices <- c(1,2)
  names(eta.choices) <- c('constant \\(\\eta\\)', label2)
  radioButtons(inputId=id,label='',
               choices= eta.choices, selected=sel,
               inline=TRUE)
}
