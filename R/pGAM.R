pGAM <- function(q,
                 mu = 1,
                 sigma = 1,
                 lower.tail = TRUE,
                 log.p = FALSE)
{
  if (any(mu <= 0))
    stop(paste("mu must be positive", "\n", ""))

  if (any(sigma <= 0))
    stop(paste("sigma must be positive", "\n", ""))

  cdf <- pgamma(q,
                shape = sigma,
                rate  = sigma/mu,
                lower.tail = lower.tail,
                log.p = log.p)

  cdf
}
