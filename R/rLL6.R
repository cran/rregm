rLL6 <- function (n, mu = 1, sigma = 1.5, nu=0.5) 
{
    if (any(mu <= 0)) 
        stop(paste("mu must be positive", "\n", ""))
    if (any(sigma <= 0)) 
        stop("sigma must be greater than 0")
  if (any(nu <= 0) | any(nu >= 1)) 
      stop("nu must be between 0 and 1")
    if (any(n <= 0)) 
        stop(paste("n must be a positive integer", "\n", ""))
    n <- ceiling(n)
    p <- runif(n)
    qLL6(p, mu = mu, sigma = sigma, nu=nu)
}
