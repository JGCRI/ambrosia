## Food demand plots

#' Plot staple, nonstaple, and total demand output from the model
#'
#' The plot will have quantity on the y-axis.  The x-axis data is passed in as a
#' separate vector, so it can be any of the input values used in the model (or
#' even in principle a variable that is only indirectly related to the input
#' values).
#'
#' @param alldata Data frame of output returned from \code{\link{food.dmnd}}.
#' @param xdata Vector of values for the x-axis.
#' @param xlabel Character string to use for the x-axis label
#' @param max.yval Maximum value to display on the y-axis.  See details.
#' @return Plot of food demand by staples and non-staples relative to income
#' @export
make.demand.plot <- function(alldata,xdata,xlabel,max.yval=NULL)
{

  xval <- value <- variable <- NULL

  totaldmnd <- alldata$Qs + alldata$Qn
  if(is.null(max.yval)) {
      max.yval <- max(totaldmnd)
  }

  ytop <- max.yval
  workset <- data.frame(staples=alldata$Qs, nonstaples=alldata$Qn, total=totaldmnd,
                        xval=xdata)
  ws.m <- reshape2::melt(workset, id='xval')
  ggplot2::qplot(data=ws.m, x=xval, y=value, colour=variable) +
    ggplot2::xlab(xlabel) + ggplot2::ylab('Q') +
    ggplot2::geom_line() + ggplot2::geom_point() + ggplot2::ylim(0,ytop)
}


#' Plot model results by year
#'
#' @param byyear.data Output from \code{\link{food.dmnd.byyear}}
#' @param pltrgn Region to plot
#' @importFrom dplyr %>%
#' @return Plot of model results by year
#' @export
make.byyear.plot <- function(byyear.data, pltrgn=NULL)
{
    rgn <- year <- Qs.Obs <- Qn.Obs <- obstype <- Qlo <- Qhi <-
        value <- variable <- Qs <- Qn <- NULL

    if(is.null(byyear.data$obstype))
        dplyr::select(byyear.data, rgn, year, Qs=Qs.Obs, Qn=Qn.Obs) %>%
          reshape2::melt(id=c('year','rgn')) -> obsdata
    else
        dplyr::select(byyear.data, rgn, year, Qs=Qs.Obs, Qn=Qn.Obs, obstype) %>%
            reshape2::melt(id=c('year','rgn','obstype')) -> obsdata

    ## Construct a curve through the model outputs.  The fit.with.err
    ## function should return a table with year, Q.predict, Qlo, and
    ## Qhi as its columns.
    perr.Qs <- fit.with.err(dplyr::select(byyear.data, year, rgn, Q=Qs))
    perr.Qn <- fit.with.err(dplyr::select(byyear.data, year, rgn, Q=Qn))

    ## Qs and Qn data points are in the same order, so it's easy to
    ## combine into a single data set.
    modeldata <- data.frame(rgn=perr.Qs$rgn, year=perr.Qs$year,
                            Qs=perr.Qs$Q.predict, Qn=perr.Qn$Q.predict) %>%
        reshape2::melt(id=c('year','rgn'))
    Qs.err <- dplyr::select(perr.Qs, rgn, year, Qlo, Qhi)
    Qn.err <- dplyr::select(perr.Qn, rgn, year, Qlo, Qhi)

    if(!is.null(pltrgn)) {
        ## filter the output to just the desired regions
        obsdata <- dplyr::filter(obsdata, rgn %in% pltrgn)
        modeldata <- dplyr::filter(modeldata, rgn %in% pltrgn)
        Qs.err <- dplyr::filter(Qs.err, rgn %in% pltrgn)
        Qn.err <- dplyr::filter(Qn.err, rgn %in% pltrgn)
    }

    baseplt <- ggplot2::ggplot(data=modeldata, ggplot2::aes(x=year)) +
      ggplot2::facet_wrap(~rgn) +
      ggplot2::ylab('Q (1000 Cal pc/day)') +
      ggplot2::geom_line(ggplot2::aes(y=value, colour=variable), size=1.5) +
      ggplot2::geom_ribbon(data=Qs.err, ggplot2::aes(x=year, ymin=Qlo, ymax=Qhi), alpha=0.2) +
      ggplot2::geom_ribbon(data=Qn.err, ggplot2::aes(x=year, ymin=Qlo, ymax=Qhi), alpha=0.2) +
      ggplot2::theme(panel.spacing=ggplot2::unit(1,'lines'))

    ## Add the points for the observed data
    if(is.null(obsdata$obstype)) {
      ## observation data not separated into training and testing
      ## sets, so just plot one type of point, but give it a legend
      ## to make it a little more clear that the dots are
      ## observations and the lines are model output.
      obsdata$obstype <- 'obs'
      baseplt +
        ggplot2::geom_point(data=obsdata, ggplot2::aes(y=value, colour=variable, shape=obstype)) +
        ggplot2::scale_shape(guide=ggplot2::guide_legend(title='Observed Data', label=FALSE))
    }
    else {
      baseplt +
        ggplot2::geom_point(data=obsdata, ggplot2::aes(y=value, colour=variable, shape=obstype)) +
        ggplot2::scale_shape(guide=ggplot2::guide_legend(title='Observed Data')) # This one includes labels for the obs types.
    }
}


#' Plot model results by per-capita income
#'
#' @param obsdata Data frame of observed food demand data
#' @param params Model parameter structure
#' @param region Regions to include in the plot.
#' @return Plot of model results by income levels
#' @export
make.byincome.plot <- function(obsdata, params, region=NULL)
{
  pcGDP <- value <- region <- variable <- NULL

  plotdata <- food.dmnd.byincome(obsdata, params, region)

  ggplot2::ggplot(data=plotdata, ggplot2::aes(x=pcGDP, y=value, color=region, variable)) +
    ggplot2::geom_point() + ggplot2::facet_wrap(~variable, ncol=2) +
    ggplot2::ylab('Q (1000 Calories/person/day)') +
    ggplot2::scale_color_brewer(type='qual', guide='legend',palette=7)

}

#' Make the by-year plot for a set of monte carlo results by sampling the distribution
#'
#' @param mc.data Monte Carlo results data
#' @param obsdata Data frame of observed food demand data
#' @param bias.correction Regional bias correction factors (default = none)
#' @param region Vector of regions to plot (default = all)
#' @param nsamp Number of samples to draw from the Monte Carlo distribution
#' @param pltrgn Regions to include in the plot.  If \code{NULL}, include them
#' all.
#' @importFrom dplyr %>%
#' @return by-year plot for a set of monte carlo results by sampling the distribution
#' @export
mc.make.byyear.plot <- function(mc.data, obsdata, bias.correction=NULL, region=NULL, nsamp=30, pltrgn=NULL)
{
  . <- NULL

  mcsamp <- mcparam.sample(mc.data, nsamp=nsamp)
  ## drop likelihood values so we have just the parameter values
  mcsamp.xl <- mcsamp
  mcsamp.xl$LL <- NULL

  fd.byyear <- apply(mcsamp.xl, 1,
                     . %>% vec2param %>% food.dmnd.byyear(obsdata, ., bias.correction, region) ) %>%
    do.call(rbind, .)

  ## Do we want to return the samples and whatnot here?  Not sure.
  make.byyear.plot(fd.byyear, pltrgn)

}

## Fit a curve through model outputs to display on plots.
fit.with.err <- function(Qdata)
{
  rgn <- year <- Q.sig <- Q.predict <- Qlo <- Qhi <- sd <- Q <- NULL

  rslt <- dplyr::group_by(Qdata, rgn, year) %>% dplyr::summarise(Q.sig=sd(Q), Q.predict=mean(Q))

  ## return value
  dplyr::mutate(rslt, Qlo=Q.predict-2*Q.sig, Qhi=Q.predict+2*Q.sig)
}


## collapse GCAM regions into categories (Asia, South America,
## etc.) so that there aren't so many of them.
simplify.region <- function(region)
{
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
