dIGMO <-
function(x, mu=1, sigma=1.5, log=FALSE)
{
  if(any(mu<=0)) stop("mu should be positive")
  if(any(sigma<=1)) stop("sigma should greater than 1")
  if(any(x<=0)) stop("x should be positive")
  alpha=sigma -1;beta=mu*sigma
  dinvgamma(x, shape=alpha, rate=beta, log=log)
}
