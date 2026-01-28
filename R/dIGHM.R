dIGHM <-
function(x, mu=1, sigma=1, log=FALSE)
{
if(any(mu<=0)) stop("mu should be positive")
if(any(sigma<=0)) stop("sigma should be positive")
if(any(x<=0)) stop("x should be positive")
alpha=sigma;beta=mu*sigma
dinvgamma(x, shape=alpha, rate=beta, log=log)
}
