#fit-omnibus
loglikZIG <- function(par,x) {
  u <- par[2]
  s <- par[3]
  p <- par[1]
  v <- s^2
  a <- u^2/v
  b <- v/u
  x1 <- x[x>0]
  n <- length(x1)
  loglik0 <- log(p) * sum(x==0)
  loglik1 <- log(1-p) * sum(x>0)
  loglik01 <- loglik0 + loglik1
  loglikr <- (a-1)*sum(log(x1)) - n*lgamma(a) - n*a*log(b) - sum(x1)/b
  loglik <- loglik01 + loglikr
  return(-loglik)
}

#Fit - abundance
loglikZIGU <- function(par,x) {
  u <- par
  s <- 1
  p <- 0.1
  v <- s^2
  a <- u^2/v
  b <- v/u
  x1 <- x[x>0]
  n <- length(x1)
  loglik0 <- log(p) * sum(x==0)
  loglik1 <- log(1-p) * sum(x>0)
  loglik01 <- loglik0 + loglik1
  loglikr <- (a-1)*sum(log(x1)) - n*lgamma(a) - n*a*log(b) - sum(x1)/b
  loglik <- loglik01 + loglikr
  return(-loglik)
}

#Fit - zero ratio
loglikZIGP <- function(par,x) {
  p <- par
  s <- 1
  u <- 0.5
  v <- s^2
  a <- u^2/v
  b <- v/u
  x1 <- x[x>0]
  n <- length(x1)
  loglik0 <- log(p) * sum(x==0)
  loglik1 <- log(1-p) * sum(x>0)
  loglik01 <- loglik0 + loglik1
  loglikr <- (a-1)*sum(log(x1)) - n*lgamma(a) - n*a*log(b) - sum(x1)/b
  loglik <- loglik01 + loglikr
  return(-loglik)
}

#Fit dispersion
loglikZIGS <- function(par,x) {
  s <- par 
  p <- 0.1
  u <- 0.5
  v <- s^2
  a <- u^2/v
  b <- v/u
  x1 <- x[x>0]
  n <- length(x1)
  loglik0 <- log(p) * sum(x==0)
  loglik1 <- log(1-p) * sum(x>0)
  loglik01 <- loglik0 + loglik1
  loglikr <- (a-1)*sum(log(x1)) - n*lgamma(a) - n*a*log(b) - sum(x1)/b
  loglik <- loglik01 + loglikr
  return(-loglik)
}

#Fit abundance and zero ratio
loglikZIGUP <- function(par,x) {
  p <- par[1]
  u <- par[2]
  s <- 1
  v <- s^2
  a <- u^2/v
  b <- v/u
  x1 <- x[x>0]
  n <- length(x1)
  loglik0 <- log(p) * sum(x==0)
  loglik1 <- log(1-p) * sum(x>0)
  loglik01 <- loglik0 + loglik1
  loglikr <- (a-1)*sum(log(x1)) - n*lgamma(a) - n*a*log(b) - sum(x1)/b
  loglik <- loglik01 + loglikr
  return(-loglik)
}

#optim example
#set initial parameters par0
#assume we have two groups, group1 and group2, with one feature

##we first fit two groups separately
op1<-optim(par=par0, fn=loglikZIG,
           lower = c(0.001,0.001,0.001), upper = c(0.999,Inf,Inf), 
           method = "L-BFGS-B", x = group1)
l1 <- -(op1$value)

op2<-optim(par=par0, fn=loglikZIG,
           lower = c(0.001,0.001,0.001), upper = c(0.999,Inf,Inf), 
           method = "L-BFGS-B", x = group2)
l2 <- -(op2$value)

l12 <- l1+l2

#we then fit two groups together
op3<-optim(par=par0, fn=loglikZIG,
           lower = c(0.001,0.001,0.001), upper = c(0.999,Inf,Inf), 
           method = "L-BFGS-B", x = c(group1,group2))
l3 <- -(op3$value)

#Finally, performance a chi-square test
chi.stat <- 2 * (l12 - l3)
p.omnibus <- pchisq(chi.stat, df = 3, lower.tail = FALSE)
