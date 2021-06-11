#Generate ZIG data varing abundance, dispersion and zero ratio
zigamma <- function(n, p0=0.5, u=0.1, s=0.5, seed=0) {
  seed=rep(0,n)
  v = s^2
  u <- u*10
  v <- v*10
  for (i in 1:n) { 
    if (runif(1, min=0, max=1)>p0)
    {seed[i]=rgamma(n=1,shape=u^2/v,rate=u/v)}
    else
    {seed[i]=0}
  }
  seed/10
}

#General zero inflated normal data
zinormal <- function(n, p0=0.5, u=0.1, s=0.5, seed=0) {
  seed=rep(0,n)
  u <- u*10
  s <- s*10
  for (i in 1:n) { 
    if (runif(1, min=0, max=1)>p0)
    {seed[i]=rnorm(1, mean=u, sd=s)}
    else
    {seed[i]=0}
  }
  seed/10
}
