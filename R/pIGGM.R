pIGGM <-
function(q, mu=1, sigma=1, lower.tail=TRUE, log.p=FALSE)
{
  if(any(mu<=0)) stop("mu should be positive")
  if(any(sigma<=0)) stop("sigma should be positive")
  if(any(q<=0)) stop("q should be positive")
  alpha= sigma + 1/2;beta=mu*sigma
  pinvgamma(q, shape=alpha, rate=beta, lower.tail=lower.tail, log.p=log.p)
}
