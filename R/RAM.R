#' @title A Robust Adaptive Metropolis (RAM) sampler using R
#' @description R implementation of RAM
#' @importFrom stats rnorm
#' @import Matrix
#' @param n number of samples
#' @param p function that returns a value proportional to the log probability density to sample from. 
#' @param init initial values vector
#' @param scale vector with the variances
#' @param log whether to sampled from a log density
#' @param adapt whether to use adaptive method
#' @param acc.rate desired acceptance rate
#' @param gamma controls the speed of adaption. Should be between 0.5 and 1. A lower gamma leads to faster adaption.
#' @param list whether to return a list (if False a matrix will be returned)
#' @param n.start iteration where the adaption starts. Only internally used.
#' @return If `list` is TRUE a list is returned otherwise only a matrix with the samples
#' @examples
#' \dontrun{
#'   #log-pdf to sample from
#'   p.log <- function(x) {
#'     B <- 0.03
#'     -x[1]^2/200 - 1/2*(x[2]+B*x[1]^2-100*B)^2
#'   }
#'   # generate samples
#'   1. non-adaptive sampling
#'   samples.1 <- RAM(n=100, p.log, init=c(0,1), scale=c(1,0.1), adapt=FALSE)
#'   2. adaptive sampling
#'   samples.1 <- RAM(n=100, p.log, init=c(0,1), scale=c(1,0.1), adapt=TRUE, acc.rate=0.2)
#' }
#' @export
RAM <- function(n, p, init, scale=rep(1, length(init)), log=TRUE,
                 adapt=!is.null(acc.rate), acc.rate=NULL, gamma=0.55, list=TRUE, n.start=0) {
  
  # checks
  if(adapt & !is.numeric(acc.rate)) stop('Argument "acc.rate" is missing!')
  if(gamma<0.5 | gamma>1) stop('Argument "gamma" must be between 0.5 and 1!')

  # number of adaption steps
  if(is.numeric(adapt)) n.adapt <- adapt
  if(adapt==TRUE) n.adapt <- Inf
  if(adapt==FALSE) n.adapt <- 0
  
  # number of parameter
  d <- length(init)
  
  # matrix to store MC chain
  X <- matrix(NA, ncol=d, nrow=n)
  colnames(X) <- names(init)
  X[1,] <- init
  
  # vector to store densities p(x)
  p.val <- rep(NA, n)
  p.val[1] <- p(X[1,])
  
  # initial S
  if(length(scale)==d) {
    M <- diag(scale)
  } else {
    M <- scale
  }
  # check
  if(ncol(M) != length(init)) stop("Length or dimension of 'init' and 'scale' do not match!")
  S <-  t(chol(M))
  
  k <- 0
  for(i in 2:n){
    # proposal value
    U <- rnorm(d)
    X.prop <- c( X[i-1,] + S %*% U )
    
    # calculate density at X.prop
    p.val.prop <- p(X.prop)
    
    # acceptance probability
    if(log) {
      alpha <- min(1, exp( p.val.prop - p.val[i-1] )) # for log density
    } else {
      alpha <- min(1, p.val.prop/p.val[i-1] )
    }
    if(!is.finite(alpha)) alpha <- 0    # if zero divided by zero
    
    # accept with P=alpha
    if(runif(1)<alpha) {
      X[i,] <- X.prop                   # accept
      p.val[i] <- p.val.prop
      k <- k+1
    } else {
      X[i,] <- X[i-1,]                  # or not
      p.val[i] <- p.val[i-1]
    }
    
    # compute new S
    ii <- i+n.start
    if(ii < n.adapt) {
      adapt.rate <-  min(5, d*ii^(-gamma))
      M <- S %*% (diag(d) + adapt.rate*(alpha - acc.rate) * U%*%t(U)/sqrt(sum(U^2))) %*% t(S)
      
      # check if M is positive definite. If not, use nearPD().
      eig <- eigen(M, only.values = TRUE)$values
      tol <- ncol(M)*max(abs(eig))*.Machine$double.eps
      
      if( !isSymmetric(M) | is.complex(eig) | !all(Re(eig)>tol) ){
        # nearPD() computes the 'nearest' positive definite matrix
        M <- as.matrix(Matrix::nearPD(M)$mat)
      }
      
      S <- t(chol(M))
      
    }
  }
  
  # calculate accpetance rate
  acceptance.rate <- round(k/(n-1), 3)
  
  if(list) {
    return(list(samples=X,
                cov.jump=M,
                n.sample=n,
                acceptance.rate=acceptance.rate,
                adaption=adapt,
                sampling.parameters=list(sample.density=p,
                                         log=log,
                                         acc.rate=acc.rate,
                                         gamma=gamma)
    ) )
  } else {
    cat("Acceptance rate:", acceptance.rate, "\n")
    return(X)
  }
}

