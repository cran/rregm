qIGMO <-
function(p, mu=1, sigma=1.5, lower.tail=TRUE, log.p=FALSE)
{
  if(any(mu<=0)) stop("mu should be positive")
  if(any(sigma<=1)) stop("sigma should greater than 1")
  if(any(p<=0 | p>=1)) stop("p should be in (0,1)")
  alpha=sigma -1;beta=mu*sigma
  qinvgamma(p, shape=alpha, rate=beta, lower.tail=lower.tail, log.p=log.p)
}
