rLNMO <- function(n, mu=1, sigma=1)
{
  if(any(mu <= 0))
    stop("mu must be positive")

  if(any(sigma <= 0))
    stop("sigma must be positive")

  n <- ceiling(n)

  rlnorm(n,
         meanlog = log(mu)+sigma,
         sdlog   = sqrt(sigma))
}


