qLL6 <- function(p, mu = 1, sigma = 1.5, nu=0.5, lower.tail = TRUE, log.p = FALSE) 
{
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  if (any(mu <= 0)) 
      stop("mu must be positive")
  if (any(sigma <= 0)) 
      stop("sigma must be greater than 0")
  if (any(nu <= 0) | any(nu >= 1)) 
      stop("nu must be between 0 and 1")
  if (any(p < 0) | any(p > 1)) 
      stop("p must be between 0 and 1")
t=mu * ((1-nu)*p/(nu*(1-p)))^(1/sigma)
  return(t)
}
