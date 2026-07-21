dLL6 <- function (x, mu = 1, sigma = 1.5, nu=0.5, log = FALSE) 
{
    if (any(mu <= 0)) 
        stop("mu must be positive")
    if (any(sigma <= 0)) 
        stop("sigma must be greater than 0")
  if (any(nu <= 0) | any(nu >= 1)) 
      stop("nu must be between 0 and 1")
    if (any(x <= 0)) 
        stop("x must be positive")
log.lik=log(sigma) - log(mu) + log1p(- nu) - log(nu) +
      (sigma + 1) * (log(mu) - log(x)) -
      2 * log1p((1/nu-1) * (mu / x)^sigma)
    if (log == FALSE) 
        fy <- exp(log.lik)
    else 
        fy <- log.lik
    fy
}
