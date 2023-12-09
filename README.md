# SA23229030

Final R package for the 'Statistical Computing 2023' course in USTC

## Usage
```R
devtools::install_github("HJHGJGHHG/SA23229030")
```

## Intro
This R package includes 3 functions (2 in R and 1 in C++), a Metropolis-Hasting sampler (MHS), a Robust Adaptive Metropolis (RAM) and a chopthin resampler. To test these functions, one can call
```R
trials <- MHS(n=1e5, alpha=3, beta=2, display=TRUE)

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

chopthin(runif(10),N=20)
```
