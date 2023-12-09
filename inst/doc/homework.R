## -----------------------------------------------------------------------------
suppressPackageStartupMessages({
    library(dplyr)
    library(ggplot2)
    library(patchwork)
    library(tidyr)
    library(gridExtra)
    library(gt)
    library(latex2exp)
    library(boot)
    library(bootstrap)
    library(DAAG)
    library(coda)
    library(purrr)
    library(microbenchmark)
    library(Rcpp)
})

## -----------------------------------------------------------------------------
v1 = c(1,2,3,4,5) # Integer vector
v2 = c('One','Two','3') # Character Vector
v3 = c(TRUE,FALSE) # Boolean Vector


v3[1] == FALSE

## -----------------------------------------------------------------------------
data <- c(1,2,3,4,5,6,7,8,9)
my_matrix <- matrix(data,nrow=3,ncol=3,byrow=TRUE)
my_matrix

## -----------------------------------------------------------------------------
my_dataframe = data.frame(
  'idx'=c(1,2,3,4), 'name'=c('A','B','C','D'),
  'age'=c(12,18,30,67), 'married'=c(FALSE,FALSE,TRUE,TRUE)
)
knitr::kable(my_dataframe)

## -----------------------------------------------------------------------------
# Load readxl package
# library('readxl')

# df_excel = read_excel('./data/test.xlsx')
# we can also use `read_xlsx` to import .xlsx file
# knitr::kable(df_excel)

## -----------------------------------------------------------------------------
# Load openxlsx package
# library('openxlsx')

# write.xlsx(my_dataframe,'./data/export.xlsx',colNames = TRUE)

## -----------------------------------------------------------------------------
# df_csv = read.csv('./data/test.csv')
# knitr::kable(df_csv)

# write.csv(my_dataframe, "./data/export.csv", row.names = TRUE)

## -----------------------------------------------------------------------------
# df_txt = read.table('./data/test.txt',sep='\t')
# knitr::kable(df_txt)

# write.table(my_dataframe, "./data/export.txt", row.names = TRUE, sep=",")

## ----pressure, echo=TRUE, fig.cap="Display local figure using knitr", out.width = '40%', fig.align='center'----
# knitr::include_graphics('./data/cat.jpg')

## ---- echo=FALSE, fig.align='center'------------------------------------------
x <- c(5,7,8,7,2,2,9,4,11,12,9,6)
y <- c(99,86,87,88,111,103,87,94,78,77,85,86)
plot(x, y, axes=FALSE)
box(); axis(1); axis(2)

## ----fig.align='center'-------------------------------------------------------
p1 <- ggplot(mtcars) + 
  geom_point(aes(mpg, disp)) + 
  ggtitle('Scatter plot')

p2 <- ggplot(mtcars) + 
  geom_boxplot(aes(gear, disp, group = gear)) + 
  ggtitle('Box plot')

p1+p2+plot_layout(widths = c(2, 1))

## ----fig.width=6,fig.height=3,fig.align='center'------------------------------
p <- ggplot(midwest, aes(
  x = area))
p + geom_histogram()

## ----fig.width=6,fig.height=3,fig.align='center'------------------------------
x <- seq(-4, 4, length=100)
y <- dnorm(x)

ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
  stat_function(fun = dnorm) +
  labs(
    titie = TeX(r"($X \sim \mathcal{N}(0,1)$)"),
    x = 'X',
    y = TeX(r'($f(x)=\frac{1}{ \sqrt[]{2\pi} } e^{-\frac{x^2}{2}}$)')
  )

## -----------------------------------------------------------------------------
my_sample <- function(data, size=1, prob=NULL){
  samples <- numeric(size)
  if (is.null(prob)){ prob <- rep(1/length(data), times=length(data)) }
  for( i in seq_len(size) ){
    U  <- runif(1)
    if(U <= prob[1]){
      samples[i] <- 1
      next
    }
    for( k in 2:length(prob) ) {
      if(sum(prob[1:(k-1)]) < U && U <= sum(prob[1:k]) )
        {samples[i] <- k} 
    }
  }
  return(samples)
}

## ---- echo=FALSE--------------------------------------------------------------
num_samples <- 100000
prob       <- c(0.1, 0.3, 0.2, 0.3, 0.1)
data <- c("0","3","4","6","9")
samples <- my_sample(data, size = num_samples, prob = prob)
sim_prob <- table(samples) / sum(table(samples))
names(sim_prob) <-  c("0","3","4","6","9")

plts <- list()

plts[[1]] <- ggplot(data.frame(),aes(x = data, y = prob )) +
             geom_col() +
             labs(x = NULL, y = "Probability", title = "True Probability Mass Function")

plts[[2]] <- ggplot(data.frame(),aes(x = data, y = sim_prob )) +
             geom_col() +
             labs(x = NULL, y = "Frequency", title = "Empirical Probability Mass Function")

grid.arrange(grobs = plts, nrow = 1)

## ---- fig.align='center'------------------------------------------------------
set.seed(1)
n <- 1000
u <- runif(n)
x <- -sign(u-1/2)*log(1-2*abs(u-1/2))
hist(x, breaks = 100, prob = TRUE, 
     main="Laplace Random Variable", xlab=NULL) #density histogram of sample
curve(1/2*exp(-abs(x)) , -6, 6, lwd = 2, xlab="", ylab="", add = T)

## ---- fig.align='center'------------------------------------------------------
set.seed(1)
p <- 3
q <- 2
n <- 1000
samples <- numeric(n)
k <- 0
while(k < n){
  u <- runif(1)
  z <- runif(1)
  if(u < z^(p-1)*(1-z)^(q-1)){
    k <- k+1
    samples[k] <- z
  }
}
hist(samples, breaks = 30, prob = TRUE, 
     main="Beta Random Variable", xlab=NULL)
curve(12*x^2*(1-x) , 0, 1, lwd = 2, xlab="", ylab="", add = T)

## ---- fig.align='center',fig.width=6,fig.height=4-----------------------------
set.seed(1)
rek_sample <- function(n){
  samples <- numeric(n)
  u1 <- runif(n, -1, 1)
  u2 <- runif(n, -1, 1)
  u3 <- runif(n, -1, 1)
  for (i in seq_len(n)){
    if ((abs(u3[i]) >= abs(u2[i])) && (abs(u3[i]) >= abs(u1[i])))
      samples[i] <- u2[i]
    else samples[i] <- u3[i]
  }
  return(samples)
}
samples <- rek_sample(10000)
hist(samples, breaks = 50, prob = TRUE,  main="Rescaled Epanechnikov Kernel Sampling", xlab=NULL)
curve(3/4*(1-x^2), -1, 1, lwd = 2, xlab="", ylab="", add = T)

## -----------------------------------------------------------------------------
rhos <- c(1,0.5,0.8)
pis <- numeric(3)
d <- 1
K <- 100
n <- 1e4
for(i in seq_len(length(rhos))){
  l <- rhos[i] * d
  estimates <- numeric(K)
  for(j in seq_len(K)){
    X <- runif(n,0,d/2)
    Y <- runif(n,0,pi/2)
    pihat <- 2*l/d/mean(l/2*sin(Y)>X)
    estimates[j] <- pihat
  }
  pis[i] <- mean(estimates)
}
data = data.frame(
  'rho'=rhos, 'estimate'=pis
)
knitr::kable(data)

## ---- echo=TRUE---------------------------------------------------------------
m <- 10000
mc <- replicate(1000, expr = {
  mean(exp(runif(m)))
})
anti <- replicate(1000, expr = {
  u <- runif(m/2)
  v <- 1 - u
  mean((exp(u) + exp(v))/2)
})
v1 <- var(mc)
v2 <- var(anti)
print(c(mean(mc), mean(anti)))
print(c(v1, v2))
print((v1-v2)/v1)

## ----fig.width=6,fig.height=3,fig.align='center'------------------------------

fun <- function(x){
  x^2*exp(-x^2/2) / sqrt(2*pi)
}
fun_1 <- function(x){
  dnorm(x, 1)
}
fun_2 <- function(x){
  dgamma(x-1, 3/2, 2) / 2.7
}
ggplot(data.frame(x = c(1, 5)), aes(x = x)) +
  stat_function(fun=fun) +
  stat_function(fun=fun_1, color='red') +
  stat_function(fun=fun_2, color='blue') +
  labs(
    titie = TeX(r"($X \sim \mathcal{N}(0,1)$)"),
    x = 'X',
    y = TeX(r'($f(x)$)')
  )

## ---- echo=TRUE---------------------------------------------------------------
m <- 10000
ie1 <- replicate(1000, expr = {
x <- sqrt(rchisq(m, 1)) + 1
f <- 2 * dnorm(x, 1)
g <- x^2 * exp(-x^2/2)/sqrt(2 * pi)
mean(g/f)
})
ie2 <- replicate(1000, expr = {
x <- rgamma(m, 3/2, 2) + 1
f <- dgamma(x-1, 3/2, 2) / 2.7
g <- x^2 * exp(-x^2/2)/sqrt(2 * pi)
mean(g/f)
})

var(ie1)/var(ie2)

## ---- echo=TRUE---------------------------------------------------------------
g <- function(x) x^2 * exp(-x^2/2)/sqrt(2 * pi)
integrate(g, lower = 1, upper = Inf)

## ---- echo=TRUE---------------------------------------------------------------
M <- 10000
k <- 5
m <- M/k
si <- numeric(k)
v <- numeric(k)
g <- function(x) exp(-x)/(1 + x^2)
f <- function(x) (k/(1 - exp(-1))) * exp(-x)
for (j in 1:k) {
u <- runif(m, (j - 1)/k, j/k)
x <- -log(1 - (1 - exp(-1)) * u)
fg <- g(x)/f(x)
si[j] <- mean(fg)
v[j] <- var(fg)
}
sqrt(mean(v))

## ---- echo=TRUE---------------------------------------------------------------
n <- 20
rootn <- sqrt(n)
t0 <- qt(c(0.025, 0.975), df = n - 1)
CI <- replicate(10000, expr = {
x <- rchisq(n, df = 2)
ci <- mean(x) + t0 * sd(x)/rootn
})
LCL <- CI[1, ]
UCL <- CI[2, ]
sum(LCL < 2 & UCL > 2)
mean(LCL < 2 & UCL > 2)

## ---- echo=TRUE---------------------------------------------------------------
set.seed(2023)
n<- 10000
alpha <- 0.05
mu0 <- 1

m <- 1000 
p <- numeric(m) 
for (j in 1:m) {
  x <- rchisq(n, df = mu0)
  ttest <- t.test(x, alternative = "two.sided", mu = mu0)
  p[j] <- ttest$p.value
  }
p.1 <-mean(p < alpha)  # Empirical Type-I Error
p.1
sqrt(p.1 * (1-p.1) / m)  # SE

## ---- echo=TRUE---------------------------------------------------------------
set.seed(2023)
n<- 10000
alpha <- 0.05
mu0 <- 1

m <- 1000 
p <- numeric(m) 
for (j in 1:m) {
  x <- runif(n,0,2)
  ttest <- t.test(x, alternative = "two.sided", mu = mu0)
  p[j] <- ttest$p.value
  }
p.2 <-mean(p < alpha)  # Empirical Type-I Error
p.2
sqrt(p.2 * (1-p.2) / m)  # SE

## ---- echo=TRUE---------------------------------------------------------------
set.seed(2023)
n<- 10000
alpha <- 0.05
mu0 <- 1

m <- 1000 
p <- numeric(m) 
for (j in 1:m) {
  x <- rexp(n,1)
  ttest <- t.test(x, alternative = "two.sided", mu = mu0)
  p[j] <- ttest$p.value
  }
p.2 <-mean(p < alpha)  # Empirical Type-I Error
p.2
sqrt(p.2 * (1-p.2) / m)  # SE

## ---- echo=TRUE---------------------------------------------------------------
M <- 1000
m <- 1000
alpha=0.1

get_corrected_p <- function(p_values, data){
  for (i in 1:length(p_values)){
    if(p_values[i]<alpha){
      if(i <= 0.95*length(p_values)){
        data[1] <- data[1]+1
        data[5] <- data[5]+1
      }
      else{
        data[2] <- data[2]+1
      }
    }
    else{
      if(i <= 0.95*length(p_values)){
        data[3] <- data[3]+1
      }
      else{
        data[4] <- data[4]+1
      }
    }
  }
  return(data)
}

get_data <-function(method){
  data <- numeric(5) # FP, TP, TN, FN, FWER
  for (k in seq_len(M)){
    p <- numeric(m)
    for(i in seq_len(0.95*m)){
      p[i] <- runif(1, min=0, max=1)
    }
    for(i in seq_len(0.05*m)){
      p[0.95*m+i] <- rbeta(1, shape1=0.1, shape2=1)
    }
    p_corrected <- p.adjust(p, method=method)
    data <- get_corrected_p(p_corrected, data)
  }
  
  return(data)
}

report_data <- function(data){
  FWER <- round(data[5] / M, 3)
  FDR <- round(data[1] / (data[1]+data[2]), 3)
  TPR <- round(data[2] / (data[2]+data[3]), 3)
  return(c(FWER, FDR, TPR))
}

data_bonf <- report_data(get_data('bonferroni'))
data_bh <- report_data(get_data('BH'))

my_dataframe = data.frame(
  'Method'=c('bonferroni','BH'), 'FWER'=c(data_bonf[1],data_bh[1]),
  'FDR'=c(data_bonf[2],data_bh[2]),'TPR'=c(data_bonf[3],data_bh[3])
)
knitr::kable(my_dataframe)

## ---- echo=TRUE---------------------------------------------------------------
lambda <- 2
B <- 1000
m <- 1000
n <- c(5, 10, 20)
get_theoretical_data <- function(num_samples){
  bias <- round(lambda/(num_samples-1), 3)
  se <- round(lambda*num_samples/((num_samples-1)*sqrt(num_samples-2)), 3)
  return(c(bias, se))
}

mean_mle <- function(samples,i){1/mean(samples[i])}

get_bootstrap_data <- function(num_samples){
  bias <- 0; se <- 0
  for(j in seq_len(m)){
    samples <- rexp(num_samples, rate=lambda)
    boot_data <- boot(samples, mean_mle, R=B)
    bias <- bias+mean(boot_data$t)-boot_data$t0
    se <- se+sd(boot_data$t)
  }
  bias <- round(bias/m, 3); se <- round(se/m, 3)
  return(c(bias, se))
}

bias_T <- numeric(length(n))
se_T <- numeric(length(n))
bias_b <- numeric(length(n))
se_b <- numeric(length(n))
for(i in seq_len(length(n))){
  num_samples <- n[i]
  data_T <- get_theoretical_data(num_samples)
  data_b <- get_bootstrap_data(num_samples)
  bias_T[i] <- data_T[1]; se_T[i] <- data_T[2]
  bias_b[i] <- data_b[1]; se_b[i] <- data_b[2]
}
my_dataframe = data.frame(
  'Samples_num'=n, 'Bias_bootstrap'=bias_b, 'Bias_theoretical'=bias_T, 'SE_bootstrap'=se_b, 'SE_theoretical'=se_T
)

knitr::kable(my_dataframe)

## -----------------------------------------------------------------------------
attach(law)
cor.stat <- function(x, i = 1:NROW(x)) {
  cor(x[i, 1], x[i, 2])
}
boot.t.ci <- function(x, B = 500, R = 100, level = 0.95, statistic) {
  x <- as.matrix(x)
  n <- nrow(x)
  stat <- numeric(B)
  se <- numeric(B)
  boot.se <- function(x, R, f) {
    x <- as.matrix(x)
    m <- nrow(x)
    th <- replicate(R, expr = {
    i <- sample(1:m, size = m, replace = TRUE)
    f(x[i, ])
    })
    return(sd(th))
  }
  for (b in 1:B) {
    j <- sample(1:n, size = n, replace = TRUE)
    y <- x[j, ]
    stat[b] <- statistic(y)
    se[b] <- boot.se(y, R = R, f = statistic)
  }
  stat0 <- statistic(x)
  t.stats <- (stat - stat0)/se
  se0 <- sd(stat)
  alpha <- 1 - level
  Qt <- quantile(t.stats, c(alpha/2, 1 - alpha/2), type = 1)
  names(Qt) <- rev(names(Qt))
  CI <- rev(stat0 - Qt * se0)
}
print(boot.t.ci(law, B = 1000, R = 25, statistic = cor.stat))

## -----------------------------------------------------------------------------
x <- aircondit[1]  # get data
meant <- function(x, i) return(mean(as.matrix(x[i, ])))
b <- boot(x, statistic = meant, R = 2000)
b

## -----------------------------------------------------------------------------
boot.ci(b, type = c("norm", "perc", "basic", "bca"))

## -----------------------------------------------------------------------------
attach(scor)
x <- as.matrix(scor)
n <- nrow(x)
theta.jack <- numeric(n)
lambda <- eigen(cov(x))$values
theta.hat <- max(lambda/sum(lambda))
for (i in 1:n) {
  y <- x[-i, ]
  s <- cov(y)
  lambda <- eigen(s)$values
  theta.jack[i] <- max(lambda/sum(lambda))
}
bias.jack <- (n - 1) * (mean(theta.jack) - theta.hat)
se.jack <- sqrt((n - 1)/n * sum((theta.jack - mean(theta.jack))^2))
c(theta.hat, bias.jack, se.jack)

## -----------------------------------------------------------------------------
# install.packages("DAAG")  # we first install DAAG package
attach(ironslag)
n <- length(magnetic) #in DAAG ironslag
N <- choose(n, 2)
e1 <- e2 <- e3 <- e4 <- numeric(N)
ij <- 1
 for (i in 1:(n - 1)) for (j in (i + 1):n) {
  k <- c(i, j)
  y <- magnetic[-k]
  x <- chemical[-k]
  J1 <- lm(y ~ x)
  yhat1 <- J1$coef[1] + J1$coef[2] * chemical[k]
  e1[ij] <- sum((magnetic[k] - yhat1)^2)
  J2 <- lm(y ~ x + I(x^2))
  yhat2 <- J2$coef[1] + J2$coef[2] * chemical[k] + J2$coef[3] * chemical[k]^2
  e2[ij] <- sum((magnetic[k] - yhat2)^2)
  J3 <- lm(log(y) ~ x)
  logyhat3 <- J3$coef[1] + J3$coef[2] * chemical[k]
  yhat3 <- exp(logyhat3)
  e3[ij] <- sum((magnetic[k] - yhat3)^2)
  J4 <- lm(log(y) ~ log(x))
  logyhat4 <- J4$coef[1] + J4$coef[2] * log(chemical[k])
  yhat4 <- exp(logyhat4)
  e4[ij] <- sum((magnetic[k] - yhat4)^2)
  ij <- ij + 1
}
c(sum(e1), sum(e2), sum(e3), sum(e4))/N

## ---- echo=TRUE---------------------------------------------------------------
my.cvm <- function(x, y, R = 999) {
  n <- length(x)
  m <- length(y)
  z <- c(x, y)
  N <- n + m
  Fn <- numeric(N)
  Gm <- numeric(N)
  for (i in 1:N) {
    Fn[i] <- mean(as.integer(z[i] <= x))
    Gm[i] <- mean(as.integer(z[i] <= y))
  }
  cvm0 <- ((n * m)/N) * sum((Fn - Gm)^2)
  cvm <- replicate(R, expr = {
  k <- sample(1:N)
  Z <- z[k]
  X <- Z[1:n]
  Y <- Z[(n + 1):N]
  for (i in 1:N) {
    Fn[i] <- mean(as.integer(Z[i] <= X))
    Gm[i] <- mean(as.integer(Z[i] <= Y))
  }
  ((n * m)/N) * sum((Fn - Gm)^2)
  })
cvm1 <- c(cvm, cvm0)
  return(list(statistic = cvm0, p.value = mean(cvm1 >= cvm0)))
}

## -----------------------------------------------------------------------------
attach(chickwts)
x <- as.vector(weight[feed == "soybean"])
y <- sort(as.vector(weight[feed == "linseed"]))
my.cvm(x, y)

## -----------------------------------------------------------------------------
maxoutliers <- function(x, y) {
  X <- x - mean(x)
  Y <- y - mean(y)
  outx <- sum(X > max(Y)) + sum(X < min(Y))
  outy <- sum(Y > max(X)) + sum(Y < min(X))
  return(max(c(outx, outy)))
}
my.maxoutliers <- function(x, y, R = 999) {
  z <- c(x, y)
  n <- length(x)
  N <- length(z)
  stats <- replicate(R, expr = {
    k <- sample(1:N)
    k1 <- k[1:n]
    k2 <- k[(n + 1):N]
    maxoutliers(z[k1], z[k2])
  })
  stat <- maxoutliers(x, y)
  stats1 <- c(stats, stat)
  tab <- table(stats1)/(R + 1)
  return(list(estimate = stat, p = mean(stats1 >= stat), freq = tab, cdf = cumsum(tab)))
}

## -----------------------------------------------------------------------------
sigma1 <- 1
sigma2 <- 2
n1 <- 10
n2 <- 100
mu <- 0
x <- rnorm(n1, mu, sigma1)
y <- rnorm(n2, mu, sigma2)
my.maxoutliers(x, y)

## ----echo=TRUE----------------------------------------------------------------
set.seed(2023)
N <- 1e6
b1<-0
b2<-1
b3<--1
my.solution <- function(N,b1,b2,b3,f0){
  x1 <- rpois(N, lambda=1)
  x2 <- rexp(N, rate=1)
  x3 <- rbinom(N,size=1,prob=0.5)
  g <- function(a){
    tmp <- exp(-a-b1*x1-b2*x2-b3*x3); p <- 1/(1+tmp)
    mean(p) - f0
  }
  solution <- uniroot(g, c(-30,0))
  return(solution$root)
}

f<-c(0.1,0.01,0.001,0.0001)
a <- numeric(length(f))
for(i in seq_len(length(f))){
  a[i] <- my.solution(N,b1,b2,b3,f0=f[i])
}
a

## ----echo=TRUE,fig.align='center'---------------------------------------------
f <- seq(1e-1, 1e-4, length=1e4)
a <- numeric(length(f))
for(i in seq_len(length(f))){
  a[i] <- my.solution(N=1e4,b1,b2,b3,f0=f[i])
}
data <- data.frame('-log f_0'=-log(f,10),'a'=a)
p1<-ggplot(data, aes(x = -log(f,10),y=a)) +
geom_line() +
labs(titie = TeX(r"($-\log f_0\; v.s\; a$)"), 
     x = TeX(r'($-\log f_0$)'), y = TeX(r'($a$)'))
p1


## ----echo=TRUE----------------------------------------------------------------
set.seed(2023)
my.random_walk <- function(N, x0, sigma) {
  x <- numeric(N)
  x[1] <- x0
  u <- runif(N)
  accept_num <- 0
  for (i in 2:N) {
    xt <- x[i - 1]
    y <- rnorm(1, xt, sigma)
    if (u[i] <= exp(abs(xt) - abs(y))){
      x[i] <- y
      accept_num = accept_num + 1
    }
    else
      x[i] <- x[i - 1]
  }
  return(list(x=x,acceptance_rate=accept_num/N))
}
N <- 10000
x0<-rnorm(1)
rw1 <- my.random_walk(N, x0, sigma=0.1)
rw2 <- my.random_walk(N, x0, sigma=1)
rw3 <- my.random_walk(N, x0, sigma=2)
rw4 <- my.random_walk(N, x0, sigma=5)
print(c(rw1$acceptance_rate, rw2$acceptance_rate, rw3$acceptance_rate, rw4$acceptance_rate))

## ----echo=FALSE,fig.align='center'--------------------------------------------
par(mar = c(1,1,1,1))
p <- ppoints(200)
y <- qexp(p, 1)
z <- c(-rev(y), y)
fx <- 0.5 * exp(-abs(z))
hist(rw1$x, breaks = 50, freq = FALSE, ylim = c(0,0.5), main = "Sigma=0.1")
lines(z, fx)
hist(rw2$x, breaks = 50, freq = FALSE, ylim = c(0,0.5), main = "Sigma=1")
lines(z, fx)
hist(rw3$x, breaks = 50, freq = FALSE, ylim = c(0,0.5), main = "Sigma=2")
lines(z, fx)
hist(rw4$x, breaks = 50, freq = FALSE, ylim = c(0,0.5), main = "Sigma=5")
lines(z, fx)
par(mfrow = c(1, 1))

## ----echo=TRUE----------------------------------------------------------------
N <- 5000
burn <- 1000
X <- matrix(0, N, 2)
corr <- 0.9  # correlation
mu1 <- 0  # zero-means
mu2 <- 0
sigma1 <- 1
sigma2 <- 1
s1 <- sqrt(1 - corr^2) * sigma1
s2 <- sqrt(1 - corr^2) * sigma2
X[1, ] <- c(mu1, mu2)
for (i in 2:N) {
  x2 <- X[i - 1, 2]
  m1 <- mu1 + corr * (x2 - mu2) * sigma1/sigma2
  X[i, 1] <- rnorm(1, m1, s1)
  x1 <- X[i, 1]
  m2 <- mu2 + corr * (x1 - mu1) * sigma2/sigma1
  X[i, 2] <- rnorm(1, m2, s2)
}
b <- burn + 1
x <- X[b:N, ]
X <- x[, 1]
Y <- x[, 2]
fit <- lm(Y ~ X)
summary(fit)

## ---- echo=FALSE,fig.align='center'-------------------------------------------
plot(fit$fit, fit$res, cex = 0.25)
abline(h = 0)

## ----echo=TRUE----------------------------------------------------------------
f <- function(x, sigma) {
if (x < 0)
  return(0)
  stopifnot(sigma > 0)
  return((x/sigma^2) * exp(-x^2/(2 * sigma^2)))
}
Gelman.Rubin <- function(psi){  # Gelman-Rubin convergence monitoring
  # We Modify the Gelman-Rubin convergence monitoring so that only the final value of $\hat R$ is computed
  psi <- as.matrix(psi)
  n <- ncol(psi)
  k <- nrow(psi)
  psi.means <- rowMeans(psi)
  B <- n * var(psi.means)
  psi.w <- apply(psi, 1, "var")
  W <- mean(psi.w)
  vhat <- W * (n - 1)/n + (B/n)
  rhat <- vhat/W
  return(rhat)
}

my.chain <- function(sigma, m, x0) {
  x <- numeric(m)
  x[1] <- x0
  u <- runif(m)
  for (i in 2:m) {
    xt <- x[i - 1]
    y <- rchisq(1, df = xt)
    num <- f(y, sigma) * dchisq(xt, df = y)
    den <- f(xt, sigma) * dchisq(y, df = xt)
    if (u[i] <= num/den)
      x[i] <- y
    else x[i] <- xt
  }
  return(x)
}
sigma <- 4
x0 <- c(1/sigma^2, 1/sigma, sigma^2, sigma^3)
k <- 4
m <- 2000
X <- matrix(0, nrow = k, ncol = m)
for (i in 1:k) X[i, ] <- my.chain(sigma, m,x0[i])
psi <- t(apply(X, 1, cumsum))
for (i in 1:nrow(psi)) psi[i, ] <- psi[i, ]/(1:ncol(psi))
rhat <- Gelman.Rubin(psi)
rhat

## ----echo=TRUE----------------------------------------------------------------
# install `coda` package
# install.packages("coda", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")
X1 <- as.mcmc(X[1, ])
X2 <- as.mcmc(X[2, ])
X3 <- as.mcmc(X[3, ])
X4 <- as.mcmc(X[4, ])
Y <- mcmc.list(X1, X2, X3, X4)
print(gelman.diag(Y))  # Gelman-Rubin diagnostic function

## -----------------------------------------------------------------------------
my.solution <- function(A) {
  # we follow the example function of Example 11.7.
  min.A <- min(A)
  A <- A - min.A  # so that v >= 0
  max.A <- max(A)
  A <- A/max(A)
  m <- nrow(A)
  n <- ncol(A)
  it <- n^3
  a <- c(rep(0, m), 1) # objective function
  A1 <- -cbind(t(A), rep(-1, n))
  b1 <- rep(0, n)
  A3 <- t(as.matrix(c(rep(1, m), 0)))
  b3 <- 1
  sx <- simplex(a = a, A1 = A1, b1 = b1, A3 = A3, b3 = b3, maxi = TRUE, n.iter = it)
  a <- c(rep(0, n), 1)
  A1 <- cbind(A, rep(-1, m))
  b1 <- rep(0, m)
  A3 <- t(as.matrix(c(rep(1, n), 0))) # constraints sum(x)=1
  b3 <- 1
  sy <- simplex(a = a, A1 = A1, b1 = b1, A3 = A3, b3 = b3, maxi = FALSE, n.iter = it)
  solution <- list(
    A = A * max.A + min.A, 
    x = sx$soln[1:m],
    y = sy$soln[1:n], 
    v = sx$soln[m + 1] * max.A +min.A)
  # returns in a list, the payoff matrix, optimal strategies, and the value of the game
  solution
}
# enter the payoff matrix
A <- matrix(c(0, -2, -2, 3, 0, 0, 4, 0, 0, 2, 0, 0, 0, -3, -3, 4, 0, 0, 2, 0, 0, 3, 0, 0, 0, -4, -4, -3, 0, -3, 0, 4, 0, 0, 5, 0, 0, 3, 0, -4, 0, -4, 0, 5, 0, 0, 3, 0, 0, 4, 0, -5, 0, -5, -4, -4, 0, 0, 0, 5, 0, 0, 6, 0, 0, 4, -5, -5, 0, 0, 0, 6, 0, 0, 4, 0, 0, 5, -6, -6, 0), 9, 9)
B <- A + 2
s <- my.solution(B)

# get optimal strategies
round(cbind(s$x, s$y), 7)

## -----------------------------------------------------------------------------
# get value of the game
s$v

## -----------------------------------------------------------------------------
dim(c(1,2,3))

## -----------------------------------------------------------------------------
a <- matrix(1:6, ncol = 3, nrow = 2)
is.matrix(a)
is.array(a)

## -----------------------------------------------------------------------------
# 0-rows data frame
data.frame(a = integer(), b = logical())

# 0-columns data frame
data.frame(row.names = 1:3)

# empty data frame with both 0 rows and 0 columns
data.frame()


## -----------------------------------------------------------------------------
scale01 <- function(x) {
rng <- range(x, na.rm = TRUE)
(x - rng[1]) / (rng[2] - rng[1])
}

## -----------------------------------------------------------------------------
numeric_df <- data.frame(
  a = c(0.3, 0.8,0.5),
  b = c(1.5, 2,3)
)

df <- data.frame(
  a = c("a", "b", "c"),
  b = c(0.3, 0.8,0.5),
  c = c(1.5, 2,3)
)
purrr::modify(numeric_df, scale01)
modify_if(df, is.numeric, scale01)

## -----------------------------------------------------------------------------
vapply(numeric_df, sd, FUN.VALUE=1.0)

## -----------------------------------------------------------------------------
vapply(df[vapply(df, is.numeric, TRUE)], sd, 1.0)

## -----------------------------------------------------------------------------
gibbs.R <- function(N, burn, a,b,n){
  # initialization
  x <- y <- rep(0, N)
  x[1] <- rbinom(1, prob = 0.5, size = n)
  y[1] <- rbeta(1, x[1] + a, n - x[1] + b)
  for (i in 2:N) {
    x[i] <- rbinom(1, prob = y[i - 1], size = n)
    y[i] <- rbeta(1, x[i] + a, n - x[i] + b)
  }
  xb <- x[(burn + 1):N]
  return(table(xb)/length(xb))
}

samples.R <- gibbs.R(N=1e5, burn=2000, a=2, b=3, n=10)

## -----------------------------------------------------------------------------
cppFunction(' 
Rcpp::NumericVector gibbs_rcpp(int N, int burn, int a, int b, int n) { 
  NumericVector x (N);  // x <- y <- rep(0, N)
  NumericVector y (N);
  
  x[0] = Rcpp::rbinom(1, n, 0.5)[0];
  y[0] = Rcpp::rbeta(1, x[1] + a, n - x[1] + b)[0];
  for(int i=1;i<N;i++){
    x[i] = Rcpp::rbinom(1, n, y[i - 1])[0];
    y[i] = Rcpp::rbeta(1, x[i] + a, n - x[i] + b)[0];
  }
  
  NumericVector xb = x[Rcpp::Range(burn + 1,N)];
  NumericVector f (Rcpp::table(xb).length());
  for(int i=1;i<table(xb).length();i++){
    f[i] = float(table(xb)[i]) / (N-burn);
  }
  return f;
}
')
s <- gibbs_rcpp(N=1e5, burn=2000, a=2, b=3, n=10)
s

## -----------------------------------------------------------------------------
N <- 1e5
burn <- 2000
a <- 2
b <- 3
n <- 10
microbenchmark(gibbs.R(N, burn, a,b,n), gibbs_rcpp(N, burn, a,b,n))

