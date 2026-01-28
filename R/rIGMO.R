rIGMO <-
function(n, mu=1, sigma=1.5)
{
  if(any(mu<=0)) stop("mu should be positive")
  if(any(sigma<=1)) stop("sigma should greater than 1")
  if(any(n<=0)) stop("n should be positive")
  alpha=sigma-1;beta=mu*sigma
  rinvgamma(n, shape=alpha, rate=beta)
}
