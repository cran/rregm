rBPGM <- function(n, mu=1, sigma=1.5)
{
  if(any(mu <= 0))
    stop("mu must be positive")

  if(any(sigma <= 0))
    stop("sigma must be positive")

  if(any(n <= 0))
    stop("n must be a positive integer")

  n <- ceiling(n)

  alpha <- mu*sigma + 1/2
  beta  <- sigma + 1/2

  z <- rbeta(n,
             shape1=alpha,
             shape2=beta)

  z/(1-z)
}
