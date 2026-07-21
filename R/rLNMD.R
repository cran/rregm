rLNMD <- function(n, mu=1, sigma=1)
{
  if(any(mu <= 0))
    stop("mu must be positive")

  if(any(sigma <= 0))
    stop("sigma must be positive")

  if(any(n <= 0))
    stop("n must be a positive integer")

  n <- ceiling(n)

  rlnorm(n,
         meanlog=log(mu),
         sdlog=sqrt(sigma))
}
