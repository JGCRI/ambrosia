library('nleqslv')

food.dmnd <- function(Ps, Pn, Y, params) {
## Function for calculating food demand using the new model
## Arguments: Ps, Pn, and Y (staple price, normal price, and pcGDP), params structure.  Ps, Pn, Y may be vectors but must all be
##            the same length
##
##     params structure:
##        params$xi    : 2x2 array of the xi elasticities
##        params$A     : Leading coefficients in the quantity calculations
##        params$yfunc : length-2 vector of functions giving Y^eta(Y) (see note below)
##
## Output: list with the following elements:
##        Qs:  vector of quantity for S
##        Qn:  vector of quantity for N
##   alpha.s:  vector of budget fraction for S
##   alpha.n:  vector of budget fraction for N
  
## Note on eta functions:  For one of the functional forms I was considering for 
## eta(Y), eta blows up, but Y^(eta(Y)) is well behaved.  Therefore, the eta functions
## need to be able to calculate not just eta(Y), but Y^(eta(Y)), so they can handle the
## limiting cases.  In pracice we probably won't be able to use these eta functions because
## the eta values they produce will likely cause the price elasticities to blow up, but I 
## wanted to be able to test them anyhow.
  
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
  
  
  list(Qs=qvals[1,], Qn=qvals[2,], alpha.s=alpharslt[1,], alpha.n=alpharslt[2,])
}

calc1eps <- function(alpha.s, alpha.n, eta.s, eta.n, xi) {
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
    ## When eta0 < 0, force a positive elasticity for Y<0.
    eta <- ifelse(Y>=1, eta0,
                  max(1-Y+eta0, eta0))
    if(calcQ) {
      Y^eta
    }
    else {
      eta
    }
  }
}


## eta.s and eta.n are alternative models for eta that vary as a function
## of Y, with eta_s and eta_n having two different models.
eta.s <- function(nu1, y0) {
  ## Return a function for calculating eta_s or Y^eta_s.  Which one 
  ## gets calculated is controlled by the parameter 'calcQ'
  ## nu1: elasticity at Y=1.
  ## y0:  value of Y for which elasticity = 0

  ## validate inputs.  Elasticity goes from + to -, so if y0<1, nu1<0.  If y0>1, nu1>0.
  ## If these conditions are violated, then the model doesn't make sense.  To protect against
  ## this, we interpret only the magnitude of nu1 as meaningful, and we set the sign 
  ## automatically
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
    ## This case is problematic.  Any value of lambda will give the requisite value at Y=1,
    ## but the shape parameter is completely undefined.  This is the price we pay for letting
    ## the user specify the shape in more natural terms.  In this case we reinterpret the nu1
    ## input as the elasticity at Y=e so as to give a well-defined result.  It's not ideal, but
    ## short of forcing users to calculate k and lambda, it's the best we can do.  In a MCMC
    ## calculation, we'll work with k and lambda directly.
    lam <- -abs(nu1)*e
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

eta.n <- function(k) {
  ## Return a function for calculating eta_n or Y^eta_n.  Which one
  ## gets calculated is controlled by the parameter 'calcQ'
  function(Y, calcQ=FALSE) {
    e.k <- exp(-k)
    delta <- 1-Y
    if (calcQ) {
      ifelse(abs(delta)>1.0e-3/k, 
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

## Set up some vectors of test values.  These can be used for exercising the
## demand function.

## logarithmically-spaced pcGDP values
##y.vals <- 10^c(seq(-1, 0, length.out=5), seq(0.1,log10(50),length.out=15))
y.vals <- 10^c(seq(-1,log10(50), length.out=20))

## evenly spaced P values
Ps.vals <- seq(0.5,2.5,by=0.1)
Pn.vals <- Ps.vals
