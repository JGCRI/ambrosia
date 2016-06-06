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
    Q <- mapply(calc1q, Ps, Pn, eps, yterm.s, yterm.n, MoreArgs=list(Acoef=params$A))
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
  qvals <- mapply(calc1q, Ps, Pn, eps, yterm.s, yterm.n, MoreArgs=list(Acoef=params$A))
  list(Qs=qvals[1,], Qn=qvals[2,], alpha.s=alpharslt[1,], alpha.n=alpharslt[2,])
}

calc1eps <- function(alpha.s, alpha.n, eta.s, eta.n, xi) {
  alpha <- c(alpha.s, alpha.n)
  eta <- c(eta.s,eta.n)
  xi - outer(eta, alpha)
}

calc1q <- function(Ps, Pn, eps, Ysterm, Ynterm, Acoef) {
  ## not vectorized:  use mapply
  c(Qs= Acoef[1] * Ps^eps[1,1] * Pn^eps[1,2] * Ysterm,
    Qn= Acoef[2] * Ps^eps[2,1] * Pn^eps[2,2] * Ynterm)
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
eta.s <- function(k) {
  ## Return a function for calculating eta_s or Y^eta_s.  Which one 
  ## gets calculated is controlled by the parameter 'calcQ'
  function(Y,calcQ=FALSE) {
    if(calcQ) {
      ifelse(Y>1.0e-2/k, Y^(k/Y), 0)
    }
    else {
      k/Y
    }
  }
}

eta.n <- function(A,k) {
  ## Return a function for calculating eta_n or Y^eta_n.  Which one
  ## gets calculated is controlled by the parameter 'calcQ'
  function(Y, calcQ=FALSE) {
    e.k <- exp(-k)
    delta <- 1-Y
    if (calcQ) {
      ifelse(abs(delta)>1.0e-2/k, 
             Y^(k/(delta)),
             e.k + 0.5*k*e.k*delta - 1.0/24.0*e.k * k*(3*k-8)*delta*delta)
    }
    else {
      k/delta
    }
  }
}

## Set up some vectors of test values.  These can be used for exercising the
## demand function.

## logarithmically-spaced pcGDP values
y.vals <- 10^c(seq(-1, 0, length.out=10), seq(0.1,log10(20),length.out=10))

## evenly spaced P values
Ps.vals <- seq(0.5,2.5,by=0.1)
Pn.vals <- Ps.vals

