library('nleqslv')

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
  rslt <- nleqslv(alphatest, falpha, method='Broyden')

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
  if(alpha.t > 1) {
    Qs <- Qs/alpha.t
    Qn <- Qn/alpha.t
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
                       e.k + 0.5*k*e.k*delta - 1.0/24.0*e.k * k*(3*k-8)*delta*delta)
        }
        else {
            k * ifelse(Y<1e-4, 1,
                       ifelse(abs(delta) > 1.0e-3/k,
                              1/delta + Y*log(Y)/(delta*delta),
                              0.5 - 1/6*delta + 1/12*delta*delta - 1/20 * delta^3))
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

    rslt
}


### The next few functions support calling the food demand function
### from a monte carlo calculation.  Since the monte carlo program is
### written in C++, it's convenient to store the prices and incomes at
### the beginning of the calculation because they don't change over
### the course of the calc, and passing data between C and R is
### costly.
mc.Ps <- 1
mc.Pn <- 1
mc.Pm <- 1
mc.Y <- 1
mc.Qs <- 1
mc.Qn <- 1
mc.sig2Qs <- 1
mc.sig2Qn <- 1

mc.setup <- function(filename)
{
    ## read observed data from input file.  Columns are:
    ##  Ps, Pn, Y, Qs, Qn, sigQs, sigQn
    obs.data <- read.csv(filename)
    
    mc.Ps <<- obs.data$Ps
    mc.Pn <<- obs.data$Pn
    mc.Y <<- obs.data$Y
    ## Pm is fixed at 1.
    mc.Pm <<- rep(1,nrow(obs.data))
    mc.Qs <<- obs.data$Qs
    mc.Qn <<- obs.data$Qn
    mc.sig2Qs <<- obs.data$sigQs^2
    mc.sig2Qn <<- obs.data$sigQn^2 
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
    ##  [A_s, A_n, xi_ss, xi_ns, xi_sn, xi_nn, nu1_n, lambda_s, k_s ]
    ## If there are only 8 parameters, then the first 7 are as above,
    ## and the last is eta_s.
    if(length(x) == 9) {
        etas <- eta.s(x[8],x[9],mc.mode=TRUE)
    }
    else if(length(x) == 8) {
        etas <- eta.constant(x[8])
    }
    else {
        msg <- paste('Invalid parameter vector.  Length must be 8 or 9.  length(x) == ', length(x))
        stop(msg)
    }

    ## construct the parameter structure
    list(A=x[1:2], yfunc=c(etas, eta.n(x[7])), xi=matrix(x[3:6], nrow=2))
}

mc.likelihood.1 <- function(x)
{
    ## Evaluate the likelihood function for a single parameter set
    params <- vec2param(x)
    dmnd <- food.dmnd(mc.Ps, mc.Pn, mc.Pm, mc.Y, params)

    ## return the log likelihood
    sum((dmnd$Qs-mc.Qs)^2/mc.sig2Qs + (dmnd$Qn-mc.Qn)^2/mc.sig2Qn)
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


## Set up some vectors of test values.  These can be used for exercising the
## demand function.

## logarithmically-spaced pcGDP values
##y.vals <- 10^c(seq(-1, 0, length.out=5), seq(0.1,log10(50),length.out=15))
y.vals <- 10^c(seq(-1,log10(50), length.out=20))

## evenly spaced P values
Ps.vals <- seq(0.5,2.5,by=0.1)
Pn.vals <- Ps.vals
Pm.vals <- rep(1.0, length(Ps.vals))


## a sample parameter structure
samp.params <- list(A=c(0.3, 0.1),
                    yfunc=c(eta.s(-0.15,0.6), eta.n(1.0)),
                    xi=matrix(c(-0.05, 0.1, 0.2, -0.5), nrow=2)
                    )
## Sample parameters in Monte Carlo representation
## same as samp.params above:
x1 <- c(0.3, 0.1, -0.05, 0.1, 0.2, -0.5, 1.0, 0.2936423, 4.5304697)
## parameters used to generate the test data:
x0 <- c(0.5, 0.35, -0.03, 0.01, 0.05, -0.4, 0.5, 0.1442695, 5.436564)
