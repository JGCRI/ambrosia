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
        y.vals <- c(seq(0.3,0.9, length.out=7), seq(1, 30, length.out=60))
    len <- length(y.vals)
    if(is.null(ps.vals))
        ps.vals <- rep(median(obsdata$Ps),len)
    if(is.null(pn.vals))
        pn.vals <- rep(median(obsdata$Pn),len)

    demand.data <- food.dmnd(ps.vals, pn.vals, y.vals, params)

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
        ggtitle('Food Demand Elasticities') + ylim(-0.25, 1.0)

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
    if(!is.null(obsdata.tst)) {
        obsdata.trn$obstype <- 'Training'
        obsdata.tst$obstype <- 'Testing'
        obsdata.all <- rbind(obsdata.trn, obsdata.tst)
        obsdata.all$obstype <- factor(obsdata.all$obstype)
    }
    else
        obsdata.all <- obsdata.trn

    plt.byyear <- mc.make.byyear.plot(mcrslt.plt, obsdata.all) +
            xlab('year') + ylab('1000 Calories/person/day') +
            theme_minimal() + scale_color_ptol(name='Demand Type', labels=c('Staples', 'Nonstaples'))

    ## rename columns and transform values to match notation in paper
    mcrslt.notation.fix <- paper1.fix.notation(mcrslt.plt)

    ## for the density plot, if the dataset is really large, sample it
    ## randomly.  The reason for this is that making a density plot
    ## with a multi-million row dataset is really slow.
    den.row.max <- 10000
    if(nrow(mcrslt.notation.fix) > den.row.max)
        mcrslt.plt <- sample_n(mcrslt.notation.fix, den.row.max)
    else
        mcrslt.plt <- mcrslt.notation.fix

    plt.density <- mcparam.density(mcrslt.plt) + theme_minimal()

    ## Not a plot, but the following code generates the 95% confidence intervals
    ci.vals <- filter(mcrslt.notation.fix, LL > quantile(LL, probs=0.01)) %>% select(-LL) %>%
        sapply(function(x) {c(min(x), max(x))})
    row.names(ci.vals) <- c('ci.low', 'ci.high')


    list(byyear=plt.byyear, density=plt.density, conf.intvl=ci.vals)

}


paper1.fix.notation <- function(pdata)
{
    ## The paper uses a slightly different notation than we use in the
    ## code.  This function transforms and renames as necessary to
    ## provide results that are consistent with the paper.  Note that
    ## you cannot use any parameters so transformed to run in the
    ## model.

    pdata$eps1n <- 2*pdata$eps1n    # paper's 'nu' is 2x our eps1n
    pdata$ks <- log(pdata$ks)       # paper's 'kappa' is log of our k_s
    rename(pdata, nu=eps1n, kappa=ks)
}

report.param.values <- function(param.vec)
{
    ## Pretty-print the parameter values contained in a vector.  Note
    ## that if you're dealing with a row from the monte carlo output
    ## data frame, you don't need this function; call
    ## paper1.fix.notation instead.

    if(length(param.vec) < 10)
        ## no likelihood value attached.  Add a placeholder.
        param.vec <- c(param.vec, rep(NA, 10-length(param.vec)))
    param <- as.data.frame(matrix(param.vec,nrow=1))
    names(param) <- namemc()
    
    paper1.fix.notation(param)
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


paper1.chisq <- function(params, obsdata, dfcorrect=0, bc=NULL)
{
    ## correct units on prices
    obsdata <- mutate(obsdata, Ps=s_usd_p1000cal*0.365, Pn=ns_usd_p1000cal*0.365) %>%
        rename(Y=gdp_pcap_thous2005usd, Qs=s_cal_pcap_day_thous, Qn=ns_cal_pcap_day_thous)

    y.vals <- obsdata$Y
    ps.vals <- obsdata$Ps
    pn.vals <- obsdata$Pn

    ## apply floor to sigma values; see discussion in paper
    sig2Qs <- pmax(obsdata$sig2Qs, 0.01)
    sig2Qn <- pmax(obsdata$sig2Qn, 0.01)

    moddata <- food.dmnd(ps.vals, pn.vals, y.vals, params, obsdata$GCAM_region_name)

    if(!is.null(bc))
        moddata <- apply.bias.corrections(moddata, bc)

    chisq <- sum( obsdata$weight*((moddata$Qs - obsdata$Qs)^2 / sig2Qs +
                                  (moddata$Qn - obsdata$Qn)^2 / sig2Qn ))

    ## Calculating the degrees of freedom, we don't subtract for the
    ## model parameters because this function is meant to be used with
    ## the testing set for cross-validation, and the parameters
    ## weren't actually fit to the testing set data.  If you are using
    ## this on data that was used in the fit, set dfcorrect to the
    ## number of fitted parameters.
    df <- sum(obsdata$weight) - dfcorrect
    pval <- pchisq(chisq, df)

    x2df <- chisq/df
    
    list(chisq=chisq, pval=pval, df=df, x2df=x2df)

}


paper1.gen.residual.data <- function(obsdata, params, bc=NULL)
{
    ## obsdata:  observed data, labeled by type
    ## params:  model parameters
    ## bc:  bias correction factors (optional)
    
    obsdata <- rename(obsdata,
                      rgn=GCAM_region_name,
                      Y=gdp_pcap_thous2005usd,
                      Qs=s_cal_pcap_day_thous,
                      Qn=ns_cal_pcap_day_thous) %>%
        mutate(Ps=0.365*s_usd_p1000cal, Pn=0.365*ns_usd_p1000cal) # unit conversion

    moddata <- split(obsdata, obsdata$rgn) %>%  # operate on each region separately for speed.
        lapply(function(d) {
                   rslt <- food.dmnd(d$Ps, d$Pn, d$Y, params, d$rgn)
                   if(!is.null(bc))
                       rslt <- apply.bias.corrections(rslt, bc)
                   mutate(d, Qs.model=rslt[['Qs']], Qn.model=rslt[['Qn']])
               }) %>%
            do.call(rbind, .) %>%
            select(rgn, Qs, Qn, Qs.model, Qn.model, obstype, expt, weight)

    moddata.s <- select(moddata, rgn, obs=Qs, model=Qs.model, obstype, expt, weight) %>%
        mutate(demand='Staples')
    moddata.n <- select(moddata, rgn, obs=Qn, model=Qn.model, obstype, expt, weight) %>%
        mutate(demand='Nonstaples')

    rbind(moddata.s,moddata.n)
}

paper1.residual.analysis <- function(mcrslt.rgn, mcrslt.yr,
                                      obsdata.rgn.trn, obsdata.rgn.tst, obsdata.yr.trn, obsdata.yr.tst)

{
    p.rgn <- mc2param(mcparam.ML(mcrslt.rgn))
    p.yr <- mc2param(mcparam.ML(mcrslt.yr))

    obsdata.rgn.trn$obstype <- 'Training'
    obsdata.rgn.tst$obstype <- 'Testing'
    obsdata.rgn.trn$expt <- 'Regional cross-validation'
    obsdata.rgn.tst$expt <- 'Regional cross-validation'

    obsdata.yr.trn$obstype <- 'Training'
    obsdata.yr.tst$obstype <- 'Testing'
    obsdata.yr.trn$expt <- 'Yearly cross-validation'
    obsdata.yr.tst$expt <- 'Yearly cross-validation'

    data <- list(obsdata.rgn.trn, obsdata.rgn.tst, obsdata.yr.trn, obsdata.yr.tst)
    params <- list(p.rgn, p.rgn, p.yr, p.yr)

    pltdata <- mapply(paper1.gen.residual.data, data, params, SIMPLIFY=FALSE) %>% do.call(rbind,.)

    scatter <- ggplot(data=pltdata, aes(x=obs, y=model, colour=demand)) +
        geom_point(size=1.5) + xlab('Observation') + ylab('Model') +
        scale_color_ptol(name='Demand Type') + geom_abline(slope=1, intercept=0, linetype=2, size=1, color='Dark Slate Grey') +
        facet_grid(obstype ~ expt) + theme_minimal()


    ## Not technically a scatter plot, but a histogram of the
    ## residuals shows a lot of what we want to get from this
    ## analysis.
    pltdata<- mutate(pltdata, resid=model - obs, resid2=resid^2)
    resid.hist <- ggplot(data=pltdata, aes(x=resid)) +
        geom_histogram(binwidth=0.05) + xlab('Residual (1000 Calories/person/day)') +
        facet_grid(obstype ~ expt) + theme_minimal()

    ## split the residuals by experiment and observation type for further analysis
    resid.split <- select(pltdata, resid, resid2, weight) %>% split(list(pltdata$obstype, pltdata$expt))
    ## RMSE for each category
    resid.rms <- sapply(resid.split, function(d) {sqrt(sum(d$weight*d$resid2)/sum(d$weight))})
    ## 95% confidence intervals for each of the four categories
    resid.conf <- sapply(resid.split, function(d) {quantile(d$resid, probs=c(0.05, 0.95))})

    ## Perform a Kolmogorov-Smirnov test on the squared residuals.
    ## The goal is to see if the residuals in the testing set are
    ## *larger* than those in the training set (we have no reason to
    ## expect them to be smaller).  If we were confident that the
    ## residuals were normally distributed we could use an F-test
    ## here, but given the chi-squared values we've been seeing, that
    ## assumption looks at least a little bit questionable, so e'll
    ## stick with K-S.
    resid.rgn.trn <- resid.split[['Training.Regional cross-validation']]
    resid.rgn.tst <- resid.split[['Testing.Regional cross-validation']]
    resid.yr.trn <- resid.split[['Training.Yearly cross-validation']]
    resid.yr.tst <- resid.split[['Testing.Yearly cross-validation']]


    ## The KS test is a test of the CDF, so "greater" means the CDF of
    ## x is greater than the CDF of y.  We want to test whether the
    ## x-values (i.e., testing residuals) are on the whole greater
    ## than the y-values (i.e., training residuals).  That means the
    ## CDF for the x-values would be *less* those for the y-values at
    ## comparable values.
    ks <- list(rgn=ks.test(resid.rgn.tst$resid2, resid.rgn.trn$resid2, alternative='less'),
               yr=ks.test(resid.yr.tst$resid2, resid.yr.trn$resid2, alternative='less'))

    list(scatter=scatter, resid.hist=resid.hist, rmse=resid.rms, resid.conf=resid.conf, ks=ks)

}

paper1.rmse.all <- function(obsdata, params)
{
    ## simple RMS error for a combined data set
    if(is.null(obsdata$obstype))
        obsdata$obstype='Training'
    if(is.null(obsdata$expt))
        obsdata$expt='Primary'
    data <- paper1.gen.residual.data(obsdata, params) %>% mutate(resid=model-obs)

    sqrt(sum(data$weight * data$resid^2)/sum(data$weight))

}


paper1.bc.plots <- function(params, obs.trn, obs.tst)
{
    ## Make plots with bias corrected data.
    
    ## NB: There is a lot of duplicated code here, but the existing
    ## functions don't do exactly what we want, and it's quicker to
    ## rewrite than to refactor.
    
    bc <- compute.bias.corrections(params, obs.trn)
    obs.trn$obstype <- 'Training'
    obs.tst$obstype <- 'Testing'
    obs.trn$expt <- 'yr'                # Not used, but paper1.gen.residual.data needs it to exist.
    obs.tst$expt <- 'yr'
    data <- list(obs.trn, obs.tst)
    
    pltdata <- mapply(paper1.gen.residual.data, data, list(params), list(bc), SIMPLIFY=FALSE) %>% do.call(rbind,.)

    scatter <- ggplot(data=pltdata, aes(x=obs, y=model, colour=demand)) +
        geom_point(size=1.5) + xlab('Observation') + ylab('Model') +
        scale_color_ptol(name='Demand Type') + geom_abline(slope=1, intercept=0, linetype=2, size=1, color='Dark Slate Grey') +
        facet_wrap(~obstype) + theme_minimal()

    resid.all <- mutate(pltdata, resid=model-obs, resid2=resid^2) %>% select(resid, resid2, weight, obstype)
    resid.split <- split(resid.all, resid.all$obstype)
    ## RMSE
    resid.rms <- sapply(resid.split, function(d) {sqrt(sum(d$weight*d$resid^2)/sum(d$weight))})
    ## 95% confidence intervals
    resid.conf <- sapply(resid.split, function(d) {quantile(d$resid, probs=c(0.05, 0.95))})

    ecdf <- ggplot(data=resid.all, aes(x=resid2, colour=obstype)) +
        stat_ecdf(size=1.5) + xlab('Residual-squared') + ylab('CDF') +
        scale_color_ptol(name='Demand Type') + theme_minimal()

    resid.hist <- ggplot(data=resid.all, aes(x=resid)) +
        geom_histogram(binwidth=0.05) + xlab('Residual') +
        facet_grid(obstype~.) + theme_minimal()

    resid.density <- ggplot(data=resid.all, aes(x=resid, colour=obstype)) +
        scale_color_ptol(name=NULL) +
        geom_density(size=1.5) + xlab('Residual') + theme_minimal()


    resid2.hist <- ggplot(data=resid.all, aes(x=resid2)) +
        geom_histogram(binwidth=0.01) + xlab('Residual-squared') +
        facet_grid(obstype~.) + theme_minimal()

    resid2.density <- ggplot(data=resid.all, aes(x=resid2, colour=obstype)) +
        geom_density(size=1.5) + xlab('Residual-squared') + theme_minimal() +
        scale_color_ptol(name=NULL)

    ## Not sure if the K-S test is really useful here, but calculate it anyhow
    resid.trn <- resid.split$Training
    resid.tst <- resid.split$Testing
    ks <- ks.test(resid.tst$resid2, resid.trn$resid2, alternative='less')

    chi2.trn <- paper1.chisq(params, obs.trn, 9, bc)
    chi2.tst <- paper1.chisq(params, obs.tst, 0, bc)
    
    list(scatter=scatter, ecdf=ecdf, hist.resid=resid.hist, den.resid=resid.density,
         hist.resid2=resid2.hist, den.resid2=resid2.density,
         chi2.trn=chi2.trn, chi2.tst=chi2.tst,
         rms=resid.rms, conf=resid.conf, ks=ks)
}

