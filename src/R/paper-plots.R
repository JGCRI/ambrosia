## Functions to generate prospective plots for Paper I.

library('ggplot2')
library('reshape2')
library('ggthemes')
library('dplyr')

srcdir <- dirname(sys.frame(1)$ofile)
source(file.path(srcdir,'food-demand.R'))
source(file.path(srcdir,'food-demand-plots.R'))

make.paper1.param.plots <- function(params, y.vals=NULL, ps.vals=NULL, pn.vals=NULL)
{
    ## Default values of plot parameters
    if(is.null(y.vals))
        y.vals <- seq(1, 30, length.out=50)
    len <- length(y.vals)
    if(is.null(ps.vals))
        ps.vals <- rep(0.022,len) # Approximate median of observational data
    if(is.null(pn.vals))
        pn.vals <- rep(0.135,len) # Approximate median of observational data

    demand.data <- food.dmnd(ps.vals, pn.vals, y.vals, params) %>%
        as.data.frame

    plt.by.gdp <- plot.qty(demand.data, y.vals, 'pcGDP-PPP (thousands)')

    elas.data <- calc.elas.actual(ps.vals, pn.vals, y.vals, params)

    plt.elas.by.gdp <- plot.elas(elas.data, y.vals, 'pcGDP-PPP (thousands)')

    hicks.data <- calc.hicks.actual(elas.data, demand.data$alpha.s, demand.data$alpha.n, demand.data$alpha.m)
    plt.hicks.by.gdp <- plot.hicks(hicks.data, y.vals, 'pcGDP-PPP (thousands)')

    list(qty.by.gdp=plt.by.gdp, elas.by.gdp=plt.elas.by.gdp, hicks.by.gdp=plt.hicks.by.gdp)
}


plot.qty <- function(demand.data, x, xlabel)
{
    pltdata <- mutate(demand.data, x=x, Qs=1000*Qs, Qn=1000*Qn, Qtot=Qs+Qn) %>%
        select(x, `Staple Calories`=Qs, `Nonstaple Calories`=Qn,
               `Total Calories`=Qtot) %>%
        melt(id='x')

    ggplot(data=pltdata, aes(x=x, y=value, color=variable)) +
        geom_line(size=1.5) + xlab(xlabel) + ylab('Calories/person/day') +
            theme_minimal() + scale_color_ptol() +
            ggtitle('Model Results: Calorie Consumption')
}

plot.elas <- function(elas.data, x, xlabel)
{
    pltdata <- mutate(elas.data, x=x) %>%
        select(x,
               `Income Elasticity (staple)`=etas,
               `Income Elasticity (nonstaple)`=etan,
               `Own-price Elasticity (staple)`=ess,
               `Own-price Elasticity (nonstaple)`=enn) %>%
        melt(id='x')

    ggplot(data=pltdata, aes(x=x, y=value, color=variable)) +
        geom_line(size=1.5) + xlab(xlabel) + ylab('') +
        theme_minimal() + scale_color_ptol(name=NULL) +
        ggtitle('Food Demand Elasticities')

}

plot.hicks <- function(hicks.data, x, xlabel)
{
    pltdata <- mutate(hicks.data, x=x) %>%
        select(x, xi.ss, xi.sn, xi.ns, xi.nn) %>%
        melt(id='x')

    ggplot(data=pltdata, aes(x=x, y=value, color=variable)) +
        geom_line(size=1.5) + xlab(xlabel) + ylab('') +
        theme_minimal() + scale_color_ptol(name=NULL) +
        ggtitle('Food Demand Hicks Elasticities')
}

make.paper1.mc.plots <- function(mcrslt, obsdata)
{
    ## Remove Europe_Non_EU region because its input data appears suspect.
    #obsdata <- filter(obsdata, GCAM_region_name != 'Europe_Non_EU')
    #obsdata$GCAM_region_name <- factor(obsdata$GCAM_region_name) # remove unused level
    plt.byyear <- mc.make.byyear.plot(mcrslt, obsdata) +
            xlab('year') + ylab('1000 Calories/person/day') +
            theme_minimal() + scale_color_ptol(name='Demand Type', labels=c('Staples', 'Nonstaples'))

}
