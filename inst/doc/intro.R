## ---- fig.align='center',fig.width=6,fig.height=4-----------------------------
library(SA23229030)

gammafunc <- function(x, alpha, beta){
  if (any(x < 0)) return (0)
  stopifnot(alpha > 0, beta > 0)
  return((x^(alpha-1)*exp(-beta*x)))
}
MHS <- function(n, alpha, beta, display=TRUE){
  x <- numeric(n)
  # sets the initial state
  x[1] <- rexp(1,rate = 1)
  u <- runif(n)
  # this defines the rest of the states
  for (i in 2:n) {
    xt <- x[i-1]
    Y <- rexp(1,rate = xt)
    # defines the acceptance rate
    num <- gammafunc(Y,alpha,beta) * dexp(xt, rate = Y)
    den <- gammafunc(xt,alpha,beta) * dexp(Y, rate = xt)
    # checks if it will be accepted
    if (u[i] <= num/den) {
      # if accepted changed to new accepted state
      x[i] <- Y
    } else {
      # if rejected the state is the same as the previous on and we try again
      x[i] <- xt
    }
  }
  if(display){
    trials <- x[round(n/10):n]
    hist(trials, xlab = 'X', main = "Beta distribution Metropolis-Hasting sampler",
         probability = TRUE,freq=F,col = "papayawhip", breaks = 100)
    curve(dgamma(x,alpha,beta),col = "tan3",lwd=2,add=TRUE)
  }
  return(x)
}

trials <- MHS(n=1e5, alpha=3, beta=2, display=TRUE)

## -----------------------------------------------------------------------------
# log-pdf to sample from
p.log <- function(x) {
  B <- 0.03
  -x[1]^2/200 - 1/2*(x[2]+B*x[1]^2-100*B)^2
}
# generate samples
# 1. non-adaptive sampling
samples.1 <- RAM(n=1000, p.log, init=c(0,1), scale=c(1,0.1), adapt=FALSE)
# 2. adaptive sampling
samples.2 <- RAM(n=1000, p.log, init=c(0,1), scale=c(1,0.1), adapt=TRUE, acc.rate=0.2)

## -----------------------------------------------------------------------------
chopthin(runif(10),N=20)

