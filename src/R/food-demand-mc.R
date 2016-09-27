### Functions for calling the food demand function from a monte carlo
### calculation.  Since the monte carlo program is written in C++,
### it's convenient to store the prices and incomes at the beginning
### of the calculation because they don't change over the course of
### the calc, and passing data between C and R can be costly.

path <- dirname(sys.frame(1)$ofile)
source(paste(path,'/food-demand.R', sep=''))

mc.obsdata <- NULL
mc.logfile <- NULL
mc.chunksize <- 10                      # see note in mc.setup
mc.splitvec <- 0                        # to be filled in later

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
    if('GCAM_region_name' %in% names(obs.data)) {
        ## The data set produced for GCAM needs some extra processing.
        obs.data <- process.gcam.data(obs.data)
    }
    else {
        ## reformat slightly:
        obs.data <- data.frame(Ps=obs.data$Ps, Pn=obs.data$Pn, Y=obs.data$Y,
                               Qs=obs.data$Qs, Qn=obs.data$Qn,
                               sig2Qs=obs.data$sigQs^2, sig2Qn=obs.data$sigQn^2,
                               weight=1.0)
    }

    ## Using nleqslv to solve the "system" causes the run time to
    ## scale nonlinearly with the number of input data.  In reality,
    ## each data point can be solved independently.  Split the data
    ## set into manageable chunks to avoid this effect.
    n <- nrow(obs.data)
    mc.splitvec <<- seq(1,n) %% ceiling(n/mc.chunksize)
    mc.obsdata <<- split(obs.data, mc.splitvec)

    if(logging) {
    	cat('End mc.setup\n', as.character(Sys.time()), '\n', file=mc.logfile)
	flush(mc.logfile)
    }

    ## Return a matrix of recommended parameter limits to the caller
    nparam <- 9
    plohi <- matrix(nrow=2, ncol=nparam)
    plohi[1,] <- c(0.001, 0.001, -2.0, -1.0, -2.0, 0.05, 0.0, 0.001, 0.1)
    plohi[2,] <- c(1.0,   1.0,   0.0,   1.0, 0.0, 1.5,   5.0,  10.0, 2.0)

    plohi
}

## minimum and maximum value for parameters:  outside of this range the model may blow up.
pmin9 <- c(0.0, 0.0, -Inf, -Inf, -Inf, 0.0, 0.0, 1e-8, 1e-6)
pmax9 <- c(Inf, Inf,  0.0,  Inf,  0.0, Inf, Inf, Inf, Inf)
## 8-parameter version
pmin8 <- c(0.0, 0.0, -Inf, -Inf, -Inf, 0.0, -Inf, 1e-6)
pmax8 <- c(Inf, Inf,  0.0,  Inf,  0.0, Inf,  Inf, Inf)

validate.params <- function(x)
{
    ## Return FALSE if the parameters are outside of allowed limits
    if(length(x)==8 && (any(x<pmin8) || any(x>pmax8)))
        FALSE
    else if(length(x)==9 && (any(x<pmin9) || any(x>pmax9)))
        FALSE
    else if(length(x) < 8 || length(x) > 9)
        FALSE
    else
        TRUE
}


vec2param <- function(x)
{
    ## Convert a vector of parameters into a params structure.  We
    ## assume that if you're using this you are doing an Monte Carlo
    ## calculation, so we set the parameters of eta.s accordingly.  We
    ## also look at the number of parameters passed in.  If it is 8,
    ## we assume you want etas = constant.  If it's 9, we assume you
    ## want etas = eta.s(lambda, k).  If it's anything else, we throw
    ## an error.
    ##
    ## The parameters in the vector are:
    ##  [A_s, A_n, xi_ss, xi_cross, xi_nn, nu1_n, lambda_s, k_s, Pm ]
    ## xi_cross is used for both xi_sn and xi_ns, forcing them to be equal.
    ##
    ## If there are only 8 parameters, then the first 6 are as above,
    ## and the next to last is eta_s.
    if(length(x) == 9) {
        etas <- eta.s(x[7],x[8],mc.mode=TRUE)
        Pm <- x[9]
    }
    else if(length(x) == 8) {
        etas <- eta.constant(x[7])
        Pm <- x[8]
    }
    else {
        msg <- paste('Invalid parameter vector.  Length must be 8 or 9.  length(x) == ', length(x))
        stop(msg)
    }

    xivals <- c(x[3], x[4], x[4], x[5])
    ## construct the parameter structure
    list(A=x[1:2], yfunc=c(etas, eta.n(x[6])), xi=matrix(xivals, nrow=2), Pm=Pm)
}

mc.eval.fd.likelihood <- function(df,params)
{
    ## Evaluate the food demand likelihood function for a subset of
    ## the observation points
    ##
    ##    df:  data frame containing the observed data inputs and outputs
    ## param:  model parameter data structure
    
    L <- -9.99e9                        # Default value, if the calc. fails
    try({
    	dmnd <- food.dmnd(df$Ps, df$Pn, df$Y, params)
        
        ## return the log likelihood.  dmnd$Q[sn] are the model
        ## outputs, df$Q[sn] are the observations, and df$sig2Q[sn]
        ## are the observational uncertainties.
        L <- -sum(df$weight*0.5*((dmnd$Qs-df$Qs)^2/df$sig2Qs + (dmnd$Qn-df$Qn)^2/df$sig2Qn))
    })
    L
}
mc.likelihood.1 <- function(x)
{
    if(validate.params(x)) {
        ## Evaluate the likelihood function for a single parameter set
        params <- vec2param(x)

        ## We've broken the data up into chunks.  Since the
        ## log-likelihood function is additive, we can apply L to each
        ## chunk and sum them up
        sum(sapply(mc.obsdata,mc.eval.fd.likelihood, params)) 
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
    
    ## construct the return data frame.  

    data.frame(Ps=Ps, Pn=Pn, Y=Y, Qs=Qs, Qn=Qn, sig2Qs=sig2Qs, sig2Qn=sig2Qn,
               weight=gcam.data$weight)
}


mc.food.dmnd.byyear <- function(obsdata, x, regions=NULL)
{
    ## Compute food demand by year for the input monte carlo parameter
    ## set x.  The x input must be in lambda-ks format, *not* eps.s,
    ## y0 format.
    ##
    ## obsdata: observations to compare to
    ## x:       input parameter values (see notes above)
    ## regions:  region(s) to compute.  If NULL, compute for all present

    vec2param(x) %>% food.dmnd.byyear(obsdata, . , region)
}

