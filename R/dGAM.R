dGAM <- function(x, mu = 1, sigma = 1, log = FALSE)
{
  if (any(mu <= 0))
    stop(paste("mu must be positive", "\n", ""))

  if (any(sigma <= 0))
    stop(paste("sigma must be positive", "\n", ""))

  fy <- dgamma(x,
               shape = sigma,
               rate  = sigma/mu,
               log   = log)

  fy
}
