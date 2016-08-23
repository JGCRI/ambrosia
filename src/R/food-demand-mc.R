### Functions for calling the food demand function from a monte carlo
### calculation.  Since the monte carlo program is written in C++,
### it's convenient to store the prices and incomes at the beginning
### of the calculation because they don't change over the course of
### the calc, and passing data between C and R can be costly.

### Most of these functions are private.  The ones that are useful to
### the outside world are:
###   * mc.setup(filename):  Setup the monte carlo calculation with the data
###                          in file <filename>
###   * mc.likelihood(x): Evaluate the likelihood function with parameter
###                       vector x
###   * namemc()

path <- dirname(sys.frame(1)$ofile)
source(paste(path,'/food-demand.R', sep=''))

mc.obsdata <- NULL
mc.logfile <- NULL
mc.chunksize <- 10                      # see note in mc.setup
mc.splitvec <- 0                        # to be filled in later
nrgn <- 0                               # number of regions, will be filled in by mc.setup

## minimum and maximum value for parameters: outside of this range the
## model may blow up.  Unlike the recommended guesses, these are hard
## limits.  Note we don't actually know at this point how many regions
## we will have, so these are a stand-in until setup is complete.
mc.parmin.struct <- c(-Inf, -Inf, -Inf, 0.0, 0.0, 1e-8) # These are the limits for the structural parameters; we'll add the scale parameters later
mc.parmax.struct <- c(0.0,  Inf,  0.0, Inf, Inf, Inf)
mc.parmin <- NULL
mc.parmax <- NULL


### Note on regionalized input vectors:
###
### The underlying demand functions expect just a single value each of
### A_s and A_n; however, when we pass a vector of parameters into the
### mc.likelihood() function we require a value of A_s and A_n for
### each region.  (We get the list of regions from the input data).
### In mc.setup we break up the observational data by region, and we
### have a shim function that looks up the region for the data subset
### being evaluated and reconstructs the old parameter vector (i.e.,
### with just a single value each for A_s and A_n) to pass to the eval
### functions.  The call chain looks like this:
###
###  mc.likelihood -> mc.likelihood.1 (via apply to each parameter set)
###                -> mc.eval.fd      (via apply to each chunk of data)
###                -> select.rgnl.coefs -> vec2param -> food.dmnd

mc.setup <- function(filename)
{
    if(exists('input.mpi.rank') && input.mpi.rank==0) {
        mc.logfile <<- file('mcpar-rlog.txt', open='wt')
	cat('Beginning mc.setup\n', as.character(Sys.time()), '\n', file=mc.logfile)
        logging <- TRUE
    }
    else {
    	 logging <- FALSE
    }
    ## read observed data from input file.  Columns are:
    ##  Ps, Pn, Y, Qs, Qn, sigQs, sigQn
    obs.data <- read.csv(filename)
    nrgn <<- levels(obs.data$GCAM_region_name) %>% length
    obs.data <- process.gcam.data(obs.data)

    ## Using nleqslv to solve the "system" causes the run time to
    ## scale nonlinearly with the number of input data.  In reality,
    ## each data point can be solved independently.  Split the data
    ## set into manageable chunks to avoid this effect.
    mc.splitvec <<- obs.data$group
    mc.obsdata <<- split(obs.data, mc.splitvec)

    if(logging) {
    	cat('End mc.setup\n', as.character(Sys.time()), '\n', file=mc.logfile)
	flush(mc.logfile)
    }

    ## Set up the parameter hard limits.  Globals mc.parmin and mc.parmax give
    ## the limits for the structure parameters.  The scale parameters
    ## are limited to >= 0.
    mc.parmin <<- c(rep(c(0,0), nrgn), mc.parmin.struct)
    mc.parmax <<- c(rep(c(Inf, Inf), nrgn), mc.parmax.struct)

    ## Return a matrix of recommended parameter guesses to the caller.
    ## These are limits for the initial guesses only, not hard limits
    ## on parameter values.
    nparam <- 6 + 2*nrgn                # 6 structure parameters, plus two levels per region
    alo <- rep(0.1,2*nrgn)              # minimum guess for A_s, A_n is 0.1
    ## maximum guess for A_s and A_n will be twice the mean value of
    ## the relevant quantity variable for that region.
    avgs <- group_by(obs.data, rgn) %>% summarise(Qsavg=mean(Qs), Qnavg=mean(Qn))
    ## ordering of the A values will be A_s by region, followed by A_n
    ## by region.  Therefore, for region r (starting from 1), A_s,r
    ## will be element r, and A_n,r will be element nrgn+r.
    ahi <- c(avgs$Qsavg, avgs$Qnavg)

    plohi <- matrix(nrow=2, ncol=nparam)
    plohi[1,] <- c(alo, -2.0, -1.0, -2.0, 0.05, 0.0, 0.001)
    plohi[2,] <- c(ahi,  0.0,  1.0, 0.0, 1.5,   5.0,  10.0)

    plohi
}


validate.params <- function(x)
{
    ## Return FALSE if the parameters are outside of allowed limits
    if(length(x) != length(mc.parmin) || any(x<mc.parmin) || any(x>mc.parmax))
        FALSE
    else
        TRUE
}


vec2param <- function(x)
{
    ## Convert a vector of parameters into a params structure.  We
    ## assume that if you're using this you are doing an Monte Carlo
    ## calculation, so we set the parameters of eta.s accordingly.  We
    ## also look at the number of parameters passed in.  If it is 7,
    ## we assume you want etas = constant.  If it's 8, we assume you
    ## want etas = eta.s(lambda, k).  If it's anything else, we throw
    ## an error.
    ##
    ## The parameters in the vector are:
    ##  [A_s, A_n, xi_ss, xi_cross, xi_nn, nu1_n, lambda_s, k_s ]
    ## xi_cross is used for both xi_sn and xi_ns, forcing them to be equal.
    ##
    ## If there are only 7 parameters, then the first 6 are as above,
    ## and the last is eta_s.
    if(length(x) == 8) {
        etas <- eta.s(x[7],x[8],mc.mode=TRUE)
    }
    else if(length(x) == 7) {
        etas <- eta.constant(x[7])
    }
    else {
        msg <- paste('Invalid parameter vector.  Length must be 8 or 9.  length(x) == ', length(x))
        stop(msg)
    }

    xivals <- c(x[3], x[4], x[4], x[5])
    ## construct the parameter structure
    list(A=x[1:2], yfunc=c(etas, eta.n(x[6])), xi=matrix(xivals, nrow=2))
}

select.rgnl.coefs <- function(x, rgn)
{
    ## x: vector of parameters, including _all_ of the regional coefficients
    ## rgn: region identifier (integer or factor)
    ##
    ## return: vector of parameters with just the regional coefficients for
    ##         the selected region.  You can pass this to vec2param.
    istrt <- 2*nrgn+1
    iend <- length(x)
    j <- as.integer(rgn)

    c(x[j], x[j+nrgn], x[istrt:iend])
}

mc.eval.fd.likelihood <- function(df,x)
{
    ## Evaluate the food demand likelihood function for a subset of
    ## the observation points
    ##
    ##    df:  data frame containing the observed data inputs and outputs
    ## param:  model parameter data structure
    ##
    ## return value:  Likelihood function for these params

    L <- -9.99e9                        # Default value, if the calc. fails
    try({
        params <- select.rgnl.coefs(x,df$rgn[1]) %>% vec2param
    	dmnd <- food.dmnd(df$Ps, df$Pn, df$Pm, df$Y, params)

        ## return the log likelihood.  dmnd$Q[sn] are the model
        ## outputs, df$Q[sn] are the observations, and df$sig2Q[sn]
        ## are the observational uncertainties.
        L <- -sum((dmnd$Qs-df$Qs)^2/df$sig2Qs + (dmnd$Qn-df$Qn)^2/df$sig2Qn)
    })
    L
}

mc.likelihood.1 <- function(x)
{
    ## Evaluate the likelihood function for a single parameter set
    if(validate.params(x)) {
        ## We've broken the data up into chunks.  Since the
        ## log-likelihood function is additive, we can apply L to each
        ## chunk and sum them up
        sum(sapply(mc.obsdata,mc.eval.fd.likelihood, x))
      }
    else {
        -9.99e9 * length(mc.obsdata)    # treat as if each chunk had returned the default value of -9.99e9
    }
}

mc.likelihood <- function(x, npset=1)
{
    ## Evaluate the likelihood function for several parameter sets.
    ## The parameter sets should be concatenated into a single vector:
    ## x <- c(x1, x2, x3)
    ## All parameter sets must have the same number of elements, so
    ## you can't combine the 8 and 9 parameter versions of the model
    ## in a single call to this function.

    xm <- matrix(x,ncol=npset)
    apply(xm, 2, mc.likelihood.1)
}

process.gcam.data <- function(gcam.data)
{
    ## Input prices are per 1000 dietary calories (presumably 2005
    ## dollars?).  We want prices in thousands of USD for a year's
    ## consumptiona at 1000 calories per day (i.e., thousand
    ## US$/365,000 cal)
    Ps <- 0.365 * gcam.data$s_usd_p1000cal
    Pn <- 0.365 * gcam.data$ns_usd_p1000cal

    ## Input quantities are in thousands of calories per capita per
    ## day.  This is the unit we want.
    Qs <- gcam.data$s_cal_pcap_day_thous
    Qn <- gcam.data$ns_cal_pcap_day_thous

    ## Input income is thousand US$ per capita per year.  This is also
    ## the unit we want.
    Y <- gcam.data$gdp_pcap_thous2005usd

    ## The GCAM data stores sigma^2 values in
    ## (thousand-cal-pc-per-day)^2, which is also the unit we want.
    ## Some of the sig^2 values we calculated came out suspiciously
    ## low, so we set a floor of 0.01 for these values.
    sig2Qs = pmax(gcam.data$sig2Qs, 0.01)
    sig2Qn = pmax(gcam.data$sig2Qn, 0.01)

    ## construct a grouping that will ensure that each group has a
    ## single region, but no group has more than mc.chunksize members.
    rgn <- gcam.data$GCAM_region_name
    rgnl <- split(rgn,rgn)
    group <- lapply(rgnl,
                    function(v) {
                        id <- 1 + (seq_along(v) %/% mc.chunksize)
                        paste(v,id,sep='.')
                    }) %>% unsplit(rgn)

    ## construct the return data frame.
    data.frame(rgn=rgn, group=group, Ps=Ps, Pn=Pn, Y=Y, Pm=1,
               Qs=Qs, Qn=Qn, sig2Qs=sig2Qs, sig2Qn=sig2Qn)
}

namemc <- function(rgnnames=NULL)
{
    ## Return the list of names for the parameters in the model.  Adds
    ##   the "LL" tag to the end to cover the log likelihood column
    ##   appened by the monte carlo code.
    ##
    ## rgnnames: Optional vector of region names.  If provided, its
    ##            length must be equal to nrgn.  If not provided, the
    ##            regions will be numbered sequentially.

    if(is.null(rgnnames)) {
        astag <- paste('As', seq(1,nrgn), sep='.')
        antag <- paste('An', seq(1,nrgn), sep='.')
    }
    else {
        astag <- paste('As', rgnnames, sep='.')
        antag <- paste('An', rgnnames, sep='.')
    }
    c(astag, antag, "xi.ss", "xi.cross", "xi.nn", "eps1n", "lambda", "ks", "LL")
}

## Some abbreviated region names.  You can use this in namemc, if you
## have all 31 regions in use
default.region.abbrevs <-
    factor(
        c('Afr-E', 'Afr-N', 'Afr-S', 'Afr-W',
          'AUS-NZ', 'BRA', 'CAN',
          'CAandC', 'C-Asia', 'CHN', 'COL',
          'EU-12', 'EU-15', 'Eur-E', 'Non-EU',
          'EFTA', 'HK-MC', 'IND', 'IDN', 'JPN', 'MEX',
          'MidEast', 'PAK', 'RUS', 'RSA', 'SAm-N',
          'SAm-S', 'S-Asia', 'KOR', 'SE-Asia',
          'USA')
        )

mc.food.dmnd.byyear <- function(obsdata, x, regions=NULL, regionalized=TRUE)
{
    ## Compute food demand by year for the input monte carlo parameter
    ## set x.  The x input must be in lambda-ks format, *not* eps.s,
    ## y0 format.
    ##
    ## obsdata: observations to compare to
    ## x:       input parameter values (see notes above)
    ## region:  region(s) to compute.  If NULL, compute for all present
    ## regionalized:  If TRUE (default), then x has separate A_s and A_n
    ##          parameters for each region.  If FALSE, then there is a
    ##          single A_s and A_n for all regions.

    if(!regionalized) {
        ## With just a single set of A avlues, we can convert to
        ## parameter structure directly and call food.dmnd.byyear.
        vec2param(x) %>% food.dmnd.byyear(obsdata, . , region)
    }
    else {
        if(is.null(regions)) {
            regions <- as.factor(levels(obsdata$GCAM_region_name))
        }
        lapply(regions,
               function(rgn) {
                   ## select regional parameters and pass to food.dmnd.byyear
                   select.rgnl.coefs(x, rgn) %>% vec2param %>%
                       food.dmnd.byyear(obsdata, . , rgn)
               }) %>%
            do.call(rbind, .)           # collect results into a single data frame
    }
}

mc.regionalize.param <- function(x)
{
    ## convert an old-style parameter set with single As and An values
    ## to the new style with regional values for those parameters.
    ## The input As and An are replicated across all regions.
    As <- x[1]
    An <- x[2]
    rest <- x[3:length(x)]

    c(rep(As,nrgn), rep(An,nrgn), rest)
}

## Sample parameters in Monte Carlo representation.  These will need
## to be regionalized by the function above.
## x1: same as samp.params in food-demand.R:
x1 <- c(0.3, 0.1, -0.05, 0.1, -0.5, 1.0, 0.2936423, 4.5304697)
## x0: parameters used to generate test data.  The test data is no
## longer used, but this is still a pretty good parameter set:
x0 <- c(0.5, 0.35, -0.03, 0.01, -0.4, 0.5, 0.1442695, 5.436564)


