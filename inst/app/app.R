library(shiny)
library(ggplot2)
library(ambrosia)

#Set default value for all parameters. These are the original values of the parameters.
etasdefault <- c(0.1, 2.77)
etandefault <- 0.49
Adefault <- c(1.28, 1.14)
Pmdefault <- 5.0
Psdefault <- 0.1
Pndefault <- 0.2
psscldefault <- 100
pnscldefault <-20


ui <- fluidPage(
  headerPanel(h1("Interactive version of \\(ambrosia\\)",align='center'),windowTitle='Food Demand Model'),
    sidebarLayout(
        sidebarPanel(
            fluidRow(h2("Model Parameters")),
            fluidRow(column(8,em("These are the parameters required by ambrosia as described in vec2param() along with the main price and income variables. In addition to the explanations of the parameter use, the names of the parameters as they appear in vec2param() are included in the parantheses."))),
            fluidRow(
                column(width=12, withMathJax(h3("Price elasticities")))
                ),
            fluidRow(
                column(width = 12, withMathJax(em("(Epsilon values for staples (ss), non-staples (nn) and cross price elasticities (cross)")))
            ),
            xi.matrix.input(),
            fluidRow(h3("Income elasticities")),
            fluidRow(column(8,h4("Staple demand"))),
            fluidRow(column(8,em("Income elasticity parameters that keep elasticity constant or use kappa (maximum income at which staple demand starts falling) and lamda (income elasticity) parameters to get elasticity values that change with incomes and prices."))),
            fluidRow(column(8,eta.selector('eta.s.select','\\(\\eta = f_s(Y)\\)',2))),
            fluidRow(
                column(3,
                       numericInput(inputId='etas', value=etasdefault[1], label='Income elasticity (lambda_s)',
                                    min=elasmin, max=elasmax, step=etastep)),
                column(4,
                       conditionalPanel('input["eta.s.select"] == 2',
                                        numericInput(inputId='y0val', label='Maximum Income level in Thousand USD (k_s)',
                                                     value=etasdefault[2], min=0.1, max=10, step=0.1)))),
            fluidRow(column(8,h4('Non-staple demand'))),
            fluidRow(column(8,em("Income elasticity parameters for non-staples that keep elasticity constant or that change with incomes and prices."))),
            fluidRow(column(8,eta.selector('eta.n.select', '\\(\\eta = f_n(Y)\\)' ,2))),
            fluidRow(column(4,
                            numericInput(inputId='etan', value=etandefault, label='elasticity value (nu1_n)',
                                         min=elasmin, max=elasmax, step=etastep))),

            fluidRow(h3('Scaling parameters')),
            fluidRow(column(8,em("Scaling parameters for staples (A_s) and non-staples (A_n) that scale demand. psscl and pnscl are additional scaling parameters applied to the prices. See the documentation of `vec2param()` for more details."))),
            fluidRow(
                column(1,
                       column.input.table(c('As','An'), Adefault, 0, 1, 0.05))
                ),
            numericInput(inputId='psscl.val', value=psscldefault, label='\\(psscl\\)', min=1, max=100, step=1),
            numericInput(inputId='pnscl.val', value=pnscldefault, label='\\(pnscl\\)', min=1, max=100, step=1),
            fluidRow(h3('Other Price and Income Variables')),
            fluidRow(column(8,em("Ps is the staple price per capita per day and Pn is the non-staple price per capita per day. Pm is the price of 'materials' which represents everything else in the economy other than food. Basically all other demands."))),
            conditionalPanel(condition='input.tab != 1',
                             sliderInput(inputId='y.val.slider', min=0, max=50.0, step=0.25, label='\\(Y\\) (Income in thousand USD)',
                                         value=1)),
            conditionalPanel(condition='input.tab != 2 && input.tab != 4',
                             sliderInput(inputId='ps.val.slider', min=0, max=5.0, step=0.02, label='\\(P_s\\) ($ per capita per day)',
                                         value=Psdefault)),
            conditionalPanel(condition='input.tab != 3',
                             sliderInput(inputId='pn.val.slider', min=0, max=5.0, step=0.02, label='\\(P_n\\) ($ per capita per day)',
                                         value=Pndefault)),
            numericInput(inputId='pm.val', value=Pmdefault, label='\\(P_m\\) ($ per capita per day)', min=0.1, max=10.0, step=0.1),
            ),

  ## Main Panel
        mainPanel(
            h2('Model Output'),
            fluidRow(column(8,em("Model outputs are available for 4 different combinations - Changing incomes with constant prices (tab 1), Changing staple prices with constant incomes, non-staple prices (tab 2), Changing non-staple prices with constant incomes and staple prices (tab 3) and Constant incomes with covarying prices for staples and non-staples (tab 4)."))),
            tabsetPanel(id="tab",
                        tabPanel(title="By pcGDP", value=1,
                                 h3('Demand (Q) in thousand calories by Income',align='center'),
                                 #tableOutput(outputId='elas.Y'),
                                 plotOutput(outputId='plot.Q.Y'),
                                 tableOutput(outputId='output.Y'),
                                 tableOutput(outputId='output.Y2')
                                 ),
                        tabPanel(title="By \\(P_s\\)", value=2,
                                 h3('Demand (Q) in thousand calories by Staple Price',align='center'),
                                 #tableOutput(outputId='elas.Ps'),
                                 plotOutput(outputId='plot.Q.Ps'),
                                 tableOutput(outputId='output.Ps'),
                                 tableOutput(outputId='output.Ps2')
                                 ),
                        tabPanel(title="By \\(P_n\\)", value=3,
                                 h3('Demand (Q) in thousand calories by Nonstaple Price', align='center'),
                                 #tableOutput(outputId='elas.Pn'),
                                 plotOutput(outputId='plot.Q.Pn'),
                                 tableOutput(outputId='output.Pn'),
                                 tableOutput(outputId='output.Pn2')
                                 ),
                        tabPanel(title="By \\(P_s, P_n\\)", value=4,
                                 h3('Demand by Staple Price, with covarying Nonstaple Price', align='center'),
                                 #tableOutput(outputId='elas.Pcov'),
                                 plotOutput(outputId='plot.Q.Pcov'),
                                 tableOutput(outputId='output.Pcov'),
                                 tableOutput(outputId='output.Pcov2')
                                 )
                        )
            )
        )  # end of sidebar layout
    ) # end of fluid page


set.model.params <-function(input)
{
  if(input$eta.s.select == 1) {
      eta.s.fn <- eta.constant(input$etas)
  }
  else {
      eta.s.fn <- eta.s(input$etas, exp(input$y0val),mc.mode = TRUE)
  }

  if(input$eta.n.select == 1) {
      eta.n.fn <- eta.constant(input$etan)
  }
  else {
      eta.n.fn <- eta.n(input$etan)
  }
  eta.fns <- c(eta.s.fn, eta.n.fn)


  list(
    xi=matrix(c(input$xiss, input$xicross, input$xicross, input$xinn), nrow=2),
    yfunc=eta.fns,
    A=c(input$As, input$An),
    Pm=input$pm.val,
    psscl=input$psscl.val,
    pnscl=input$pnscl.val)
}

## data frames to hold persistent results.
ydata  <- data.frame(Ps=1, Pn=1, Y=1, alpha.s=0, alpha.n=0,Qs=0,Qn=0)
psdata <- data.frame(Ps=1, Pn=1, Y=1, alpha.s=0, alpha.n=0,Qs=0,Qn=0)
pndata <- data.frame(Ps=1, Pn=1, Y=1, alpha.s=0, alpha.n=0,Qs=0,Qn=0)
pcovdata <- data.frame(Ps=1, Pn=1, Y=1, alpha.s=0, alpha.n=0,Qs=0,Qn=0)

yelas  <- data.frame(ess=1,esn=1,etas=1,deltas=1,ens=1,enn=1,etan=1,deltan=1)
pselas <- data.frame(ess=1,esn=1,etas=1,deltas=1,ens=1,enn=1,etan=1,deltan=1)
pnelas <- data.frame(ess=1,esn=1,etas=1,deltas=1,ens=1,enn=1,etan=1,deltan=1)
pcovelas <- data.frame(ess=1,esn=1,etas=1,deltas=1,ens=1,enn=1,etan=1,deltan=1)

server <- function(input, output) {
  model.data <- reactive({
    ## Compute results for income change
    params <- set.model.params(input)
    if(input$tab == 1) {
      ps <- rep(input$ps.val.slider, length(y.vals))
      pn <- rep(input$pn.val.slider, length(y.vals))
      rslt <- food.dmnd(ps,pn,y.vals,params)
      ydata <<- data.frame(Ps=ps, Pn=pn, Y=y.vals, alpha.s=rslt$alpha.s, alpha.n=rslt$alpha.n,
                           Qs=rslt$Qs, Qn=rslt$Qn, Qm=rslt$Qm)
      erslt <- calc.elas.actual(ps, pn, y.vals, params, rslt)
      exi <- calc.hicks.actual(erslt, rslt$alpha.s, rslt$alpha.n, rslt$alpha.m)
      yelas <<- data.frame(ess=erslt$ess, esn=erslt$esn, esm=erslt$esm, etas=erslt$etas,
                           ens=erslt$ens, enn=erslt$enn, enm=erslt$enm, etan=erslt$etan,
                           deltas=(erslt$ess + erslt$esn + erslt$esm + erslt$etas),
                           deltan=(erslt$ens + erslt$enn + erslt$enm + erslt$etan),
                           xiss=exi$xi.ss, xinn=exi$xi.nn, ximm=exi$xi.mm,
                           xins=exi$xi.ns, xisn=exi$xi.sn,
                           xinswt=exi$xi.ns.wt, xisnwt=exi$xi.sn.wt)
    }

    ## compute results for staple price change.    The relationship isn't great, but it gives a better sense of what is happening
    if(input$tab == 2) {
      yvals <- rep(input$y.val.slider,length(Ps.vals))
      pn <- rep(input$pn.val.slider, length(Ps.vals))
      rslt <- food.dmnd(Ps.vals, pn, yvals, params)
      psdata <<- data.frame(Ps=Ps.vals, Pn=pn, Y=yvals, alpha.s=rslt$alpha.s, alpha.n=rslt$alpha.n,
                            Qs=rslt$Qs, Qn=rslt$Qn, Qm=rslt$Qm)
      erslt <- calc.elas.actual(Ps.vals, pn, yvals, params, rslt)
      exi <- calc.hicks.actual(erslt, rslt$alpha.s, rslt$alpha.n, rslt$alpha.m)
      pselas <<- data.frame(ess=erslt$ess, esn=erslt$esn, esm=erslt$esm, etas=erslt$etas,
                            ens=erslt$ens, enn=erslt$enn, enm=erslt$enm, etan=erslt$etan,
                            deltas=(erslt$ess + erslt$esn + erslt$esm + erslt$etas),
                            deltan=(erslt$ens + erslt$enn + erslt$enm + erslt$etan),
                            xiss=exi$xi.ss, xinn=exi$xi.nn, ximm=exi$xi.mm,
                            xins=exi$xi.ns, xisn=exi$xi.sn,
                            xinswt=exi$xi.ns.wt, xisnwt=exi$xi.sn.wt)
    }

    ## compute results for nonstaple price change
    if(input$tab == 3) {
      yvals <- rep(input$y.val.slider,length(Pn.vals))
      ps <- rep(input$ps.val.slider, length(Pn.vals))
      rslt <- food.dmnd(ps, Pn.vals, yvals, params)
      pndata <<- data.frame(Ps=ps, Pn=Pn.vals, Y=yvals, alpha.s=rslt$alpha.s, alpha.n=rslt$alpha.n,
                            Qs=rslt$Qs, Qn=rslt$Qn, Qm=rslt$Qm)
      erslt <- calc.elas.actual(ps, Pn.vals, yvals, params, rslt)
      exi <- calc.hicks.actual(erslt, rslt$alpha.s, rslt$alpha.n, rslt$alpha.m)
      pnelas <<- data.frame(ess=erslt$ess, esn=erslt$esn, esm=erslt$esm, etas=erslt$etas,
                            ens=erslt$ens, enn=erslt$enn, enm=erslt$enm, etan=erslt$etan,
                            deltas=(erslt$ess + erslt$esn + erslt$esm + erslt$etas),
                            deltan=(erslt$ens + erslt$enn + erslt$enm + erslt$etan),
                            xiss=exi$xi.ss, xinn=exi$xi.nn, ximm=exi$xi.mm,
                            xins=exi$xi.ns, xisn=exi$xi.sn,
                            xinswt=exi$xi.ns.wt, xisnwt=exi$xi.sn.wt)
  }

    ## Compute results for covarying Ps and Pn.  Because rising staple
    ## prices affect nonstaple prices, we compute the Pn test values
    ## using a linear relationship with Ps.  The slider bar sets Pm
    ## for Ps==1, and we assume that Pm(Ps=0) == 0.1 (which is more or
    ## less supported by the data).  The relationship isn't great, but
    ## it gives a better sense of what is happening in realistic model
    ## cases than keeping Pn constant with varying Pm.
    if(input$tab == 4) {
        yvals <- rep(input$y.val.slider,length(Ps.vals))
        slope <- input$pn.val.slider - 0.1
        pn <- 0.1 + slope*Ps.vals
        rslt <- food.dmnd(Ps.vals, pn, yvals, params)
        pcovdata <<- data.frame(Ps=Ps.vals, Pn=pn, Y=yvals, alpha.s=rslt$alpha.s, alpha.n=rslt$alpha.n,
                                Qs=rslt$Qs, Qn=rslt$Qn, Qm=rslt$Qm)
        erslt <- calc.elas.actual(Ps.vals, pn, yvals, params, rslt)
        exi <- calc.hicks.actual(erslt, rslt$alpha.s, rslt$alpha.n, rslt$alpha.m)
        pcovelas <<- data.frame(ess=erslt$ess, esn=erslt$esn, esm=erslt$esm, etas=erslt$etas,
                                ens=erslt$ens, enn=erslt$enn, enm=erslt$enm, etan=erslt$etan,
                                deltas=(erslt$ess + erslt$esn + erslt$esm + erslt$etas),
                                deltan=(erslt$ens + erslt$enn + erslt$enm + erslt$etan),
                                xiss=exi$xi.ss, xinn=exi$xi.nn, ximm=exi$xi.mm,
                                xins=exi$xi.ns, xisn=exi$xi.sn,
                                xinswt=exi$xi.ns.wt, xisnwt=exi$xi.sn.wt)
    }
    maxplot <- ceiling(max(ydata$Qs + ydata$Qn))
    ## return all of the above
    list(ydata=ydata, psdata=psdata, pndata=pndata, pcovdata=pcovdata, maxplot=maxplot, yelas=yelas, pselas=pselas, pnelas=pnelas, pcovelas=pcovelas)
})

    output$output.Y <- renderTable({
      model.data()$ydata
    },caption = "Table with demand side variables*",
    caption.placement = getOption("xtable.caption.placement", "top"))
    output$output.Y2 <- renderText({"* P values = prices (staples, non-staples), Q values = demands in thousand calories (staples, non-staples, materials), Y = income in thousand USD, alpha values = expenditure shares (staples, non-staples)"
    })
    output$plot.Q.Y <- renderPlot({
      make.demand.plot(model.data()$ydata,y.vals,'per-capita Income (Thousand USD)', model.data()$maxplot)
    })
    #output$elas.Y <- renderTable({model.data()$yelas})

    output$output.Ps <- renderTable({
      model.data()$psdata
    },caption = "Table with demand side variables*",
    caption.placement = getOption("xtable.caption.placement", "top"))
    output$output.Ps2 <- renderText({"* P values = prices (staples, non-staples), Q values = demands in thousand calories (staples, non-staples, materials), Y = income in thousand USD, alpha values = expenditure shares (staples, non-staples)"
    })
    output$plot.Q.Ps <- renderPlot({
      make.demand.plot(model.data()$psdata,Ps.vals,'Price (staples) in $ per capita per day', model.data()$maxplot)
    })
    #output$elas.Ps <- renderTable({model.data()$pselas})

    output$output.Pn <- renderTable({
      model.data()$pndata
    },caption = "Table with demand side variables*",
    caption.placement = getOption("xtable.caption.placement", "top"))

    output$output.Pn2 <- renderText({"* P values = prices (staples, non-staples), Q values = demands in thousand calories (staples, non-staples, materials), Y = income in thousand USD, alpha values = expenditure shares (staples, non-staples)"
    })

    output$plot.Q.Pn <- renderPlot({
      make.demand.plot(model.data()$pndata,Pn.vals,'Price (nonstaples) in  $ per capita per day', model.data()$maxplot)
    })
    #output$elas.Pn <- renderTable({model.data()$pnelas})

  output$output.Pcov <- renderTable({model.data()$pcovdata},caption = "Table with demand side variables*",
                                    caption.placement = getOption("xtable.caption.placement", "top"))

  output$output.Pcov2 <- renderText({"* P values = prices (staples, non-staples), Q values = demands in thousand calories (staples, non-staples, materials), Y = income in thousand USD, alpha values = expenditure shares (staples, non-staples)"
  })

  output$plot.Q.Pcov <- renderPlot({make.demand.plot(model.data()$pcovdata, Ps.vals, 'Price (staples, nonstaples covarying)',
                                                     model.data()$maxplot)})
  #output$elas.Pcov <- renderTable({model.data()$pcovelas})
}

shinyApp(ui = ui, server = server)

