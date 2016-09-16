## Functions to generate prospective plots for Paper I.

library('ggplot2')
library('reshape2')
library('ggthemes')
library('dplyr')

srcdir <- dirname(sys.frame(1)$ofile)
source(file.path(srcdir,'food-demand.R'))
source(file.path(srcdir,'food-demand-plots.R'))
source(file.path(srcdir,'mcpar-analysis.R'))

make.paper1.param.plots <- function(params, obsdata, y.vals=NULL, ps.vals=NULL, pn.vals=NULL)
{
    ## correct units on observational data prices: This factor takes
    ## us from daily cost in dollars (for a 1000-calorie allotment) to
    ## yearly cost in thousands of dollars (for same).
    obsdata <- mutate(obsdata, Ps=s_usd_p1000cal*0.365, Pn=ns_usd_p1000cal*0.365) %>%
        rename(Y=gdp_pcap_thous2005usd, Qs.obs=s_cal_pcap_day_thous, Qn.obs=ns_cal_pcap_day_thous)

    ## Default values of plot parameters
    if(is.null(y.vals))
        y.vals <- seq(1, 30, length.out=50)
    len <- length(y.vals)
    if(is.null(ps.vals))
        ps.vals <- rep(median(obsdata$Ps),len)
    if(is.null(pn.vals))
        pn.vals <- rep(median(obsdata$Pn),len)

    demand.data <- food.dmnd(ps.vals, pn.vals, y.vals, params) %>%
        as.data.frame

    plt.by.gdp <- plot.qty(demand.data, y.vals, 'pcGDP-PPP (thousands)')

    elas.data <- calc.elas.actual(ps.vals, pn.vals, y.vals, params)

    plt.elas.by.gdp <- plot.elas(elas.data, y.vals, 'pcGDP-PPP (thousands)')

    hicks.data <- calc.hicks.actual(elas.data, demand.data$alpha.s, demand.data$alpha.n, demand.data$alpha.m)
    plt.hicks.by.gdp <- plot.hicks(hicks.data, y.vals, 'pcGDP-PPP (thousands)')

    ## scatter plots of observations vs model outputs.

    plt.scatter <- plot.model.obs.scatter(obsdata, params)

    list(qty.by.gdp=plt.by.gdp, elas.by.gdp=plt.elas.by.gdp, hicks.by.gdp=plt.hicks.by.gdp, scatter=plt.scatter)
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

plot.model.obs.scatter <- function(obsdata, params)
{
    obsdata <- rename(obsdata, rgn=GCAM_region_name)
    rgn <- obsdata$rgn
    pltdata <- split(obsdata, rgn) %>%
        lapply(function(d) {
                   rslt <- food.dmnd(d$Ps, d$Pn, d$Y, params)
                   mutate(d, Qs.model=rslt$Qs, Qn.model=rslt$Qn)
               }) %>%
        do.call(rbind, . )

    ## need to turn this into a paired dataset
    if(is.null(pltdata$obstype)) {
        pltdata$obstype <- 'obs'
        label.obstype <- FALSE
    }
    else
        label.obstype <- TRUE

    pltdata.s <- select(pltdata, obs=Qs.obs, model=Qs.model, obstype) %>% mutate(demand='Staples')
    pltdata.n <- select(pltdata, obs=Qn.obs, model=Qn.model, obstype) %>% mutate(demand='Nonstaples')
    pltdata.all <- rbind(pltdata.s, pltdata.n)

    plt <- ggplot(data=pltdata.all, aes(x=obs, y=model, colour=demand, shape=obstype)) +
        geom_point(size=1.5) + xlab('Observation') + ylab('Model') +
        scale_color_ptol(name='Demand Type') + geom_abline(slope=1, intercept=0, linetype=2, size=1.5)

    if(label.obstype)
        plt + scale_shape(name='Observation Type')
    else
        plt + scale_shape(guide=FALSE)

}

make.paper1.mc.plots <- function(mcrslt, obsdata.trn, obsdata.tst=NULL)
{
    ## Make plots for paper1 that involve the entire MC posterior distribution
    ## (i.e., not just the best set of parameters)
    ##
    ## mcrslt - results of the Monte Carlo calculation
    ## obsdata.trn - observed data training set
    ## obsdata.tst - observed data testing set (optional).
    ##
    ## The Monte Carlo Results will be filtered to remove rows with a
    ## likelihood such that Lmax-L > 2*nrow(obsdata).  These results
    ## are typically a long tail of transients from the beginning of
    ## the calculation.  They are generally harmless, but they mess up
    ## the scales of the density plots.

    mcrslt.plt <- filter(mcrslt, LL > quantile(LL, probs=0.01))

    ## Create a merged data set with training and testing observations marked as such
    obsdata.trn$obstype <- 'Training'
    if(!is.null(obsdata.tst)) {
        obsdata.tst$obstype <- 'Testing'
        obsdata.all <- rbind(obsdata.trn, obsdata.tst)
    }
    else
        obsdata.all <- obsdata.trn
    obsdata.all$obstype <- factor(obsdata.all$obstype)

    plt.byyear <- mc.make.byyear.plot(mcrslt.plt, obsdata.all) +
            xlab('year') + ylab('1000 Calories/person/day') +
            theme_minimal() + scale_color_ptol(name='Demand Type', labels=c('Staples', 'Nonstaples'))


    ## for the density plot, if the dataset is really large, sample it
    ## randomly.  The reason for this is that making a density plot
    ## with a multi-million row dataset is really slow.
    den.row.max <- 10000
    if(nrow(mcrslt.plt) > den.row.max)
        mcrslt.plt <- sample_n(mcrslt.plt, den.row.max)
    ## Also, convert the eta.s parameters to eps-y0 notation.
    mcrslt.plt <- lamks2epsy0(mcrslt.plt)

    plt.density <- mcparam.density(mcrslt.plt) + theme_minimal()

    ## Not a plot, but the following code generates the 95% confidence intervals
    ci.vals <- filter(mcrslt, LL > quantile(LL, probs=0.01)) %>% select(-LL) %>%
        sapply(function(x) {c(min(x), max(x))})
    row.names(ci.vals) <- c('ci.low', 'ci.high')
    

    list(byyear=plt.byyear, density=plt.density, conf.intvl=ci.vals)

}

make.paper1.obs.plots <- function(obsdata)
{
    ## plots that require just the observational data

    ## histogram of sigma values
    pltdata <- mutate(obsdata, Nonstaples=sqrt(sig2Qn), Staples=sqrt(sig2Qs)) %>%
        select(GCAM_region_name, year, Nonstaples, Staples) %>%
        melt(id=c('GCAM_region_name', 'year'))

    ggplot(data=pltdata, aes(x=value)) +
        facet_grid(variable ~ .) + geom_histogram(binwidth=0.02) + theme_minimal() +
        ggtitle('Imputed sigma values') + xlab('sigma (1000 cal/person/day)')

}
