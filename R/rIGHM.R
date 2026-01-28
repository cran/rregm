rIGHM <-
function(n, mu=1, sigma=1)
{
if(any(mu<=0)) stop("mu should be positive")
if(any(sigma<=0)) stop("sigma should be positive")
if(any(n<=0)) stop("n should be positive")
alpha=sigma;beta=mu*sigma
rinvgamma(n, shape=alpha, rate=beta)
}
