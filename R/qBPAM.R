qBPAM <- function(p,
                  mu = 1,
                  sigma = 1.5,
                  lower.tail = TRUE,
                  log.p = FALSE)
{
  if(any(mu <= 0))
    stop(paste("mu must be positive","\n",""))

  if(any(sigma <= 0))
    stop(paste("sigma must be positive","\n",""))

  if(any(p <= 0) | any(p >= 1))
    stop(paste("p must be between 0 and 1","\n",""))

  alpha <- mu*sigma
  beta  <- sigma + 1

  z <- qbeta(p,
             shape1 = alpha,
             shape2 = beta,
             lower.tail = lower.tail,
             log.p = log.p)

  z/(1-z)
}
