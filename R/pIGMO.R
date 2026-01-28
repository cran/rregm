pIGMO <-
function(q, mu=1, sigma=1.5, lower.tail=TRUE, log.p=FALSE)
{
  if(any(mu<=0)) stop("mu should be positive")
  if(any(sigma<=1)) stop("sigma should greater than 1")
  if(any(q<=0)) stop("q should be positive")
  alpha= sigma -1;beta=mu*sigma
  pinvgamma(q, shape=alpha, rate=beta, lower.tail=lower.tail, log.p=log.p)
}
