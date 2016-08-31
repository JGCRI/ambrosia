library('nleqslv')
library('dplyr')

food.dmnd <- function(Ps, Pn, Pm, Y, params) {
## Function for calculating food demand using the new model
## Arguments: Ps, Pn, and Y (staple price, normal price, and pcGDP), params structure.  Ps, Pn, Y may be vectors but must all be
##            the same length
##
##     params structure:
##        params$xi    : 2x2 array of the xi elasticities
##        params$A     : Leading coefficients in the quantity calculations
##        params$yfunc : length-2 vector of functions giving Y^eta(Y) (see note below)
##
##     Note that we don't need elasticity parameters for the materials
##     component because we calculate Qm as a residual
##
## Output: list with the following elements:
##        Qs:  vector of quantity for S
##        Qn:  vector of quantity for N
##        Qm:  vector of quantity for M
##   alpha.s:  vector of budget fraction for S
##   alpha.n:  vector of budget fraction for N
##   alpha.m:  vector of budget fraction for M

## Note on eta functions:  For one of the functional forms I was considering for
## eta(Y), eta blows up, but Y^(eta(Y)) is well behaved.  Therefore, the eta functions
## need to be able to calculate not just eta(Y), but Y^(eta(Y)), so they can handle the
## limiting cases.  In pracice we probably won't be able to use these eta functions because
## the eta values they produce will likely cause the price elasticities to blow up, but I
## wanted to be able to test them anyhow.


    ## Normalize income and prices to Pm
    Ps <- Ps/Pm
    Pn <- Pn/Pm
    Y  <- Y/Pm

  # get eta values
  eta.s <- params$yfunc[[1]](Y,FALSE)
  eta.n <- params$yfunc[[2]](Y,FALSE)

  # Get Y^eta values.  We have to let the eta object calculate them because it may
  # need to do something special near Y=0 or Y=1
  yterm.s <- params$yfunc[[1]](Y,TRUE)
  yterm.n <- params$yfunc[[2]](Y,TRUE)

  ## create the equation that we are going to solve for alpha.  Here alpha is
  ## a 2xN vector, where N is the number of Y values.  alpha[1,] == alpha.s
  ## and alpha[2,] == alpha.n
  falpha <- function(alpha) {
    ## Calculate constant-price elasticities, leave in a list for calc1q below
    eps <- mapply(calc1eps, alpha[1,], alpha[2,], eta.s, eta.n, MoreArgs=list(xi=params$xi),
                  SIMPLIFY=FALSE)
    ## Calculate quantities Q[1,] is Qs and Q[2,] is Qn
    Q <- mapply(calc1q, Ps, Pn, Y, eps, yterm.s, yterm.n, MoreArgs=list(Acoef=params$A))
    ## alpha.out = P*Q/Y
    alpha.out <- alpha
    alpha.out[1,] <- Ps*Q[1,]/Y
    alpha.out[2,] <- Pn*Q[2,]/Y

    ## output of this function is alpha - alpha.out.  Solving for the roots of this
    ## equation will give us a self-consistent alpha
    alpha - alpha.out
  }
  ## Now, use a nonlinear equation solver to find a consistent set of alpha values.
  alphatest <- matrix(0.5, nrow=2, ncol=length(Y))
  alphatest[2,] <- 0.05
  ## Solve for alpha
  rslt <- nleqslv(alphatest, falpha, method='Broyden', control=list(maxit=200))

  ## calculate resulting Q values
  alpharslt <- matrix(rslt$x, nrow=2)
  eps <- mapply(calc1eps, alpharslt[1,], alpharslt[2,], eta.s, eta.n, MoreArgs=list(xi=params$xi),
                SIMPLIFY=FALSE)
  qvals <- mapply(calc1q, Ps, Pn, Y, eps, yterm.s, yterm.n, MoreArgs=list(Acoef=params$A))
  qs <- qvals[1,]
  qn <- qvals[2,]
  ## calculate Qm as the budget residual.
    resid <- Y - (Ps*qs + Pn*qn)
    qm <-  resid / Pm
    alpha.m <- resid / Y

  list(Qs=qs, Qn=qn, Qm=qm, alpha.s=alpharslt[1,], alpha.n=alpharslt[2,], alpha.m=alpha.m)
}

calc1eps <- function(alpha.s, alpha.n, eta.s, eta.n, xi) {
    ## calculate the exponents in the demand equation.  This is an
    ## approximation to the Slutsky equations, inasmuch as we use
    ## these as the exponents directly, instead of solving for
    ## exponents that produce these values as the elasticities.

    ## First apply symmetry condition.  This means that the xi.sn
    ## value will be ignored.  Also, set a floor on the terms to
    ## ensure that the function is well-behaved.
    alphamin <- 1.0e-2
    xi[1,2] <- max(alpha.n, alphamin)/max(alpha.s,alphamin) * xi[2,1]

    ## Now calculate the epsilon matrix using the Slutsky equation.
  alpha <- c(alpha.s, alpha.n)
  eta <- c(eta.s,eta.n)
  xi - outer(eta, alpha)
}

calc1q <- function(Ps, Pn, Y, eps, Ysterm, Ynterm, Acoef) {
  ## not vectorized:  use mapply
  Qs <- Acoef[1] * Ps^eps[1,1] * Pn^eps[1,2] * Ysterm
  Qn <- Acoef[2] * Ps^eps[2,1] * Pn^eps[2,2] * Ynterm

  ## Check the budget constraint
  alpha.s <- Ps*Qs/Y
  alpha.n <- Pn*Qn/Y
  alpha.t <- alpha.s + alpha.n
  food.budget <- 1                      # maximum budget fraction for total food.
  if(alpha.t > 1) {
      ## Food consumption exceeds the budget constraint; reduce
      ## consumption to stay within budget.  Reduce nonstaples first,
      ## since they will normally be a less efficient source of
      ## calories than staples.
      if(alpha.s < food.budget) {
          alpha.n <- food.budget-alpha.s
      }
      else {
          alpha.n <- 0
          alpha.s <- food.budget
      }
      Qs <- alpha.s * Y/Ps
      Qn <- alpha.n * Y/Pn
  }
  c(Qs=Qs, Qn=Qn)
}

eta.constant <- function(eta0) {
  ## Return an eta function where eta is constant with a specified value.  This still
  ## uses the calcQ interface described below
  function(Y, calcQ=FALSE) {
    if(calcQ) {
      Y^eta0
    }
    else {
      eta0
    }
  }
}


## eta.s and eta.n are alternative models for eta that vary as a function
## of Y, with eta_s and eta_n having two different models.
eta.s <- function(nu1, y0, mc.mode=FALSE) {
    ## Return a function for calculating eta_s or Y^eta_s.  Which one
    ## gets calculated is controlled by the parameter 'calcQ'
    ## nu1: elasticity at Y=1.
    ## y0:  value of Y for which elasticity = 0
    ## mc.mode: Monte Carlo mode.  If true, then treat the first two
    ##          parameters as a direct specification of lambda and k
    ##          (respectively).  Because parts of the parameter space
    ##          are not accessible in the nu1-y0 formulation, when
    ##          running monte carlo calcs we specify the coefficients
    ##          directly.

    if(mc.mode) {
        lam <- nu1
        k <- y0
    }
    else {
        ## validate inputs.  Elasticity goes from + to -, so if y0<1,
        ## nu1<0.  If y0>1, nu1>0.  If these conditions are violated,
        ## then the model doesn't make sense.  To protect against
        ## this, we interpret only the magnitude of nu1 as meaningful,
        ## and we set the sign automatically
        if(y0<=1) {
            ## see below for special handling when y0 = 1
            nu1 <- -abs(nu1)
        }
        else {
            nu1 <- abs(nu1)
        }

        ## We need to caclulate the coefficients k and lambda.  Q = (kY)^(lambda/Y)
        e <- exp(1.0)
        k <- e/y0
        if(abs(1-y0) > 1e-4) {
            lam <- nu1/(1-log(k))
        }
        else {
            ## This case is problematic.  Any value of lambda will
            ## give the requisite value at Y=1, but the shape
            ## parameter is completely undefined.  This is the price
            ## we pay for letting the user specify the shape in more
            ## natural terms.  In this case we reinterpret the nu1
            ## input as the elasticity at Y=e so as to give a
            ## well-defined result.  It's not ideal, but short of
            ## forcing users to calculate k and lambda, it's the best
            ## we can do.  In a MCMC calculation, we'll work with k
            ## and lambda directly.
            lam <- -abs(nu1)*e
        }
    }
    ## Calculate y1, the value of y where the elasticity is 1.  1-ln(k*y1) = y1/lam
    y1 <- calc.etas.y1(k,lam)
    Qy1 <- (k*y1)^(lam/y1) / y1       # match-up condition:  qty at y=y1, divided by y
    scl <- k^(-lam)                   # scale factor gives Y(1) = 1.
    function(Y,calcQ=FALSE) {
        if(calcQ) {
            scl * ifelse(Y>y1, (k*Y)^(lam/Y),
                         Qy1*Y)
        }
        else {
            ifelse(Y>y1,
                   lam*(1-log(k*Y))/Y,   # y1 is the value of Y for which this expression = 1
                   1)
        }
    }
}

eta.n <- function(nu1) {
    ## Return a function for calculating eta_n or Y^eta_n.  Which one
    ## gets calculated is controlled by the parameter 'calcQ'.

    ## We don't have a mc.mode parameter for this function because it
    ## is well-behaved when specified in terms of nu1, so we just
    ## stick with that.

    ## Arguments:
    ##   nu1 : elasticity at Y=1.  Evidently, k == 2*nu1
    k <- 2*nu1

    function(Y, calcQ=FALSE) {
        e.k <- exp(-k)
        delta <- 1-Y
        scl <- 1/e.k
        if (calcQ) {
            scl*ifelse(abs(delta)>1.0e-3/k,
                       Y^(k/(delta)),
                       e.k - 0.5*k*e.k*delta + 1.0/24.0*e.k * k*(3*k-8)*delta*delta)
        }
        else {
            k * ifelse(Y<1e-4, 1,
                       ifelse(abs(delta) > 1.0e-3/k,
                              1/delta + Y*log(Y)/(delta*delta),
                              0.5 + 1/6*delta + 1/12*delta*delta + 1/20 * delta^3))
        }
    }
}

calc.etas.y1 <- function(k, lam)
{
  ## Given k and lambda, find the value of Y for which the elasticity in eta.s is 1.
  ## This is expressible in terms of the Lambert W-function, but the R package
  ## for calculating that function won't install properly.  It's easier just to
  ## solve for it.
  ##
  ## We'll probably only use this function with single values, but it's vectorized
  ## just in case. Using nleqslv is kind of overkill, but it does keep this function
  ## simple
  ffunc <- function(y) {1-log(k*y)-y/lam}
  jfunc <- function(y) {diag(-k/y-1/lam, nrow=length(k))}
  rslt <- nleqslv(x=rep(1,length(k)), fn=ffunc, jac=jfunc)
  rslt$x
}

calc.elas.actual <- function(Ps,Pn,Pm,Y, params, basedata=NULL)
{
    ## Given a set of prices and incomes, and model
    ## parameters,calculate the elasticities using numerical
    ## derivatives.  Optionally, you can pass the model results for
    ## the base values, if you've already calculated them.
    if(is.null(basedata)) {
        basedata <- food.dmnd(Ps, Pn, Pm, Y, params)
    }

    ## size of finite difference step
    h <- 0.001

    ## Calculate Ps elasticities
    psdelta <- Ps + h
    psh <- 1.0/(psdelta - Ps)           # Using psdelta-ps instead of h helps with roundoff error.
    psdata <- food.dmnd(psdelta, Pn, Pm, Y, params)
    eps.ss <- (psdata$Qs - basedata$Qs) * psh * Ps/basedata$Qs
    eps.ns <- (psdata$Qn - basedata$Qn) * psh * Ps/basedata$Qn
    eps.ms <- (psdata$Qm - basedata$Qm) * psh * Ps/basedata$Qm

    ## Calculate Pn elasticities
    pndelta <- Pn + h
    pnh <- 1.0/(pndelta - Pn)
    pndata <- food.dmnd(Ps, pndelta, Pm, Y, params)
    eps.sn <- (pndata$Qs - basedata$Qs) * pnh * Pn/basedata$Qs
    eps.nn <- (pndata$Qn - basedata$Qn) * pnh * Pn/basedata$Qn
    eps.mn <- (pndata$Qm - basedata$Qm) * pnh * Pn/basedata$Qm

    ## Calculate Pm elasticities
    pmdelta <- Pm + h
    pmh <- 1.0/(pmdelta-Pm)
    pmdata <- food.dmnd(Ps, Pn, pmdelta, Y, params)
    eps.sm <- (pmdata$Qs - basedata$Qs) * pmh * Pm/basedata$Qs
    eps.nm <- (pmdata$Qn - basedata$Qn) * pmh * Pm/basedata$Qn
    eps.mm <- (pmdata$Qm - basedata$Qm) * pmh * Pm/basedata$Qm

    ## Calculate income elasticities
    ydelta <- Y + h
    yh <- 1.0/(ydelta - Y)
    ydata <- food.dmnd(Ps, Pn, Pm, ydelta, params)
    eta.s <- (ydata$Qs - basedata$Qs) * yh * Y/basedata$Qs
    eta.n <- (ydata$Qn - basedata$Qn) * yh * Y/basedata$Qn
    eta.m <- (ydata$Qm - basedata$Qm) * yh * Y/basedata$Qm

    data.frame(ess=eps.ss, ens=eps.ns, ems=eps.ms, esn=eps.sn, enn=eps.nn, emn=eps.mn,
               esm=eps.sm, enm=eps.nm, emm=eps.mm, etas=eta.s, etan=eta.n, etam=eta.m)

}

calc.hicks.actual <- function(eps, alpha.s, alpha.n, alpha.m)
{
    ## Calculate the actual Hicks elasticities using the Slutsky equation.
    ## Arguments:
    ##   eps   - elasticity values calculated by calc.elas.actual
    ## alpha.s - budget fraction for staples
    ## alpha.n - budget fraction for nonstaples
    ## alpha.m - budget fraction for materials
    xi.ss <- eps$ess + alpha.s * eps$etas
    rslt <- data.frame(xi.ss=xi.ss)
    rslt$xi.sn <- eps$esn + alpha.n * eps$etas
    rslt$xi.sm <- eps$esm + alpha.m * eps$etas

    rslt$xi.ns <- eps$ens + alpha.s * eps$etan
    rslt$xi.nn <- eps$enn + alpha.n * eps$etan
    rslt$xi.nm <- eps$enm + alpha.m * eps$etan

    rslt$xi.ms <- eps$ems + alpha.s * eps$etam
    rslt$xi.mn <- eps$emn + alpha.n * eps$etam
    rslt$xi.mm <- eps$emm + alpha.m * eps$etam

    rslt$xi.sn.wt <- rslt$xi.sn * alpha.s
    rslt$xi.ns.wt <- rslt$xi.ns * alpha.n

    rslt
}


food.dmnd.byyear <- function(obsdata, params, region=NULL)
{
    ## Plot food demand by year for in input model using the observed
    ## prices and incomes for a given region.  If region == NULL, do
    ## it for all regions and concatenate the result
    if(is.null(region)) {
        ## run this function for all regions and collect the results
        ## into a single table.
        levels(obsdata$GCAM_region_name) %>%
            lapply(. %>% food.dmnd.byyear(obsdata, params, .)) %>%
            do.call(rbind, .)
    }
    else {
        filter(obsdata, GCAM_region_name==region) %>%
            mutate(Ps=0.365*s_usd_p1000cal, Pn=0.365*ns_usd_p1000cal) %>%
                select(year, Y=gdp_pcap_thous2005usd, Ps, Pn,
                       Qs.Obs=s_cal_pcap_day_thous, Qn.Obs=ns_cal_pcap_day_thous) -> indata
        Pm <- rep_len(1,nrow(indata))
        rslt <- food.dmnd(indata$Ps, indata$Pn, Pm, indata$Y, params)
        rslt$year <- indata$year
        rslt$rgn <- region
        rslt$Qs.Obs <- indata$Qs.Obs
        rslt$Qn.Obs <- indata$Qn.Obs
        as.data.frame(rslt)
    }
}


lamks2epsy0 <- function(df)
{
    ## convert the eta.s k and lambda parameters to nu1 and y0
    if(!('lambda' %in% names(df)) || !('ks' %in% names(df))) {
        warning('data frame does not contain lambda & ks vars.')
    }
    else {
        e <- exp(1.0)
        ## add y0 and nu1 columns
        df$y0 <- e / df$ks
        df$eps1s <- df$lambda * (1-log(df$ks))
        ## drop ks and lambda columns.  Arrange for LL to still be at the end
        lltmp <- df$LL
        df$ks <- NULL
        df$lambda <- NULL
        df$LL <- NULL
        df$LL <- lltmp
    }
    df
}


## Set up some vectors of test values.  These can be used for exercising the
## demand function.

## logarithmically-spaced pcGDP values
##y.vals <- 10^c(seq(-1, 0, length.out=5), seq(0.1,log10(50),length.out=15))
y.vals <- 10^c(seq(-1,log10(50), length.out=20))

## evenly spaced P values
Ps.vals <- seq(0.05,2.0,length.out=20)
Pn.vals <- seq(0.1, 2.5, length.out=20)
Pm.vals <- rep(1.0, length(Ps.vals))


## a sample parameter structure
samp.params <- list(A=c(0.3, 0.1),
                    yfunc=c(eta.s(-0.15,0.6), eta.n(1.0)),
                    xi=matrix(c(-0.05, 0.1, 0.1, -0.5), nrow=2)
                    )
## Sample parameters in Monte Carlo representation
## same as samp.params above:
x1 <- c(0.3, 0.1, -0.05, 0.1, 0.2, -0.5, 1.0, 0.2936423, 4.5304697)
## parameters used to generate the test data:
x0 <- c(0.5, 0.35, -0.03, 0.01, 0.05, -0.4, 0.5, 0.1442695, 5.436564)
