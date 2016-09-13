## Food demand plots
library(ggplot2)
library(reshape2)
library('dplyr')
library('RColorBrewer')

make.demand.plot <- function(alldata,xdata,xlabel,max.yval)
{
  ## alldata: results of the food demand calculation
  ## xcolumn: name of the column for the X plot
  totaldmnd <- alldata$Qs + alldata$Qn
  ytop <- max(c(max.yval,totaldmnd))
  workset <- data.frame(staples=alldata$Qs, nonstaples=alldata$Qn, total=totaldmnd,
                        xval=xdata)
  ws.m <- melt(workset, id='xval')
  qplot(data=ws.m, x=xval, y=value, colour=variable) + xlab(xlabel) + ylab('Q') +
    geom_line() + geom_point() + ylim(0,ytop)
}


make.byyear.plot <- function(byyear.data)
{
    if(is.null(byyear.data$obstype))
        select(byyear.data, rgn, year, Qs=Qs.Obs, Qn=Qn.Obs) %>% melt(id=c('year','rgn')) -> obsdata 
    else
        select(byyear.data, rgn, year, Qs=Qs.Obs, Qn=Qn.Obs, obstype) %>%
            melt(id=c('year','rgn','obstype')) -> obsdata

    ## Construct a curve through the model outputs.  The fit.with.err
    ## function should return a table with year, Q.predict, Qlo, and
    ## Qhi as its columns.
    perr.Qs <- fit.with.err(select(byyear.data, year, rgn, Q=Qs))
    perr.Qn <- fit.with.err(select(byyear.data, year, rgn, Q=Qn))

    ## Qs and Qn data points are in the same order, so it's easy to
    ## combine into a single data set.
    modeldata <- data.frame(rgn=perr.Qs$rgn, year=perr.Qs$year,
                            Qs=perr.Qs$Q.predict, Qn=perr.Qn$Q.predict) %>%
        melt(id=c('year','rgn'))
    Qs.err <- select(perr.Qs, rgn, year, Qlo, Qhi)
    Qn.err <- select(perr.Qn, rgn, year, Qlo, Qhi)

    baseplt <- ggplot(data=modeldata, aes(x=year)) +
        facet_wrap(~rgn) +
        ylab('Q (1000 Cal pc/day)') +
        geom_line(aes(y=value, colour=variable), size=1.5) +
        geom_ribbon(data=Qs.err, aes(x=year, ymin=Qlo, ymax=Qhi), alpha=0.2) +
        geom_ribbon(data=Qn.err, aes(x=year, ymin=Qlo, ymax=Qhi), alpha=0.2) +
        theme(panel.margin=unit(1,'lines'))

    ## Add the points for the observed data
    if(is.null(obsdata$obstype)) {
        ## observation data not separated into training and testing
        ## sets, so just plot one type of point, but give it a legend
        ## to make it a little more clear that the dots are
        ## observations and the lines are model output.
        obsdata$obstype <- 'obs'
        baseplt + 
            geom_point(data=obsdata, aes(y=value, colour=variable, shape=obstype)) +
            scale_shape(guide=guide_legend(title='Observed Data', label=FALSE))
    }
    else {
        baseplt +
            geom_point(data=obsdata, aes(y=value, colour=variable, shape=obstype)) +
            scale_shape(guide=guide_legend(title='Observed Data')) # This one includes labels for the obs types.
    }
}


make.byincome.plot <- function(obsdata, params, region=NULL)
{
    plotdata <- food.dmnd.byincome(obsdata, params, region)

    ggplot(data=plotdata, aes(x=pcGDP, y=value, color=region, variable)) +
        geom_point() + facet_wrap(~variable, ncol=2) +
        ylab('Q (1000 Calories/person/day)') +
        scale_color_brewer(type='qual', guide='legend',palette=7)

}

mc.make.byyear.plot <- function(mc.data, obsdata, region=NULL, nsamp=30)
{
    ## Make the by-year plot for a set of monte carlo results by
    ## sampling the distribution

    ## NB: you have to have sourced the mcpar-analysis functions from
    ## the mcpar project to use this function.

    ## sample the parameters and apply the food demand function to the samples.
    mcsamp <- mcparam.sample(mc.data, nsamp=nsamp)
    ## drop likelihood values so we have just the parameter values
    mcsamp.xl <- mcsamp
    mcsamp.xl$LL <- NULL

    fd.byyear <- apply(mcsamp.xl, 1,
                       . %>% vec2param %>% food.dmnd.byyear(obsdata, ., region) ) %>%
        do.call(rbind, .)

    ## Do we want to return the samples and whatnot here?  Not sure.
    make.byyear.plot(fd.byyear)

}

fit.with.err <- function(Qdata)
{

    rslt <- group_by(Qdata, rgn, year) %>% summarise(Q.sig=sd(Q), Q.predict=mean(Q))

    ## return value
    mutate(rslt, Qlo=Q.predict-2*Q.sig, Qhi=Q.predict+2*Q.sig)
}


simplify.region <- function(region)
{
    ## collapse GCAM regions into categories (Asia, South America,
    ## etc.) so that there aren't so many of them.
    ifelse( grepl('Africa',region), 'Africa',
      ifelse( grepl('Europe|EU',region) | region == 'Russia', 'Europe',
        ifelse( grepl('Asia', region) |
                   region %in% c('Australia_NZ', 'HongKong_Macau',
                                 'Japan','Middle East', 'Pakistan',
                                 'South Korea'),
               'Asia-Pacific',
          ifelse( grepl('America', region) |
                     region %in% c('Brazil', 'Canada',
                                   'Colombia', 'Mexico'),
                 'Americas',
                 as.character(region)))))
}
