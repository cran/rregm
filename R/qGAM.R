qGAM <- function(p,
                 mu = 1,
                 sigma = 1,
                 lower.tail = TRUE,
                 log.p = FALSE)
{
  if (any(mu <= 0))
    stop(paste("mu must be positive", "\n", ""))

  if (any(sigma <= 0))
    stop(paste("sigma must be positive", "\n", ""))

  if (any(p <= 0) | any(p >= 1))
    stop(paste("p must be between 0 and 1", "\n", ""))

  q <- qgamma(p,
              shape = sigma,
              rate  = sigma/mu,
              lower.tail = lower.tail,
              log.p = log.p)

  q
}
