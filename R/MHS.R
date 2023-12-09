#' @title A Beta distribution Metropolis-Hasting sampler (MHS) using R
#' @description This is a setup function, a Beta distribution Metropolis-Hasting sampler using R
#' @import Rcpp
#' @importFrom stats runif rexp dexp dgamma
#' @importFrom graphics curve hist
#' @param n the number of samples
#' @param alpha Beta distribution alpha param
#' @param beta Beta distribution beta param
#' @param display whether to display a generated histogram
#' @return a random sample of size \code{N}
#' @examples
#' \dontrun{
#'     trials <- MHS(n=1e5, alpha=3, beta=2, display=TRUE)
#' }
#' @export
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

# defining the gamma function up to a constant
gammafunc <- function(x, alpha, beta){
  if (any(x < 0)) return (0)
  stopifnot(alpha > 0, beta > 0)
  return((x^(alpha-1)*exp(-beta*x)))
}