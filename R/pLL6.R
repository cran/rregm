pLL6 <- function(q, mu = 1, sigma = 1.5, nu=0.5, lower.tail = TRUE, log.p = FALSE) 
{
    if (any(mu <= 0)) 
        stop("mu must be positive")
    if (any(sigma <= 0)) 
        stop("sigma must be greater than 0")
  if (any(nu <= 0) | any(nu >= 1)) 
      stop("nu must be between 0 and 1")
    if (any(q < 0)) 
        stop("q must be positive")
	cdf=exp(-log1p((1/nu-1) * (mu / q)^sigma))
    if (!lower.tail) cdf <- 1 - cdf
    if (log.p) cdf <- log(cdf)
    return(cdf)
}
