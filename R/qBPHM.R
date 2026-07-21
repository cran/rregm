qBPHM <- function(p,
                  mu=1,
                  sigma=1.5,
                  lower.tail=TRUE,
                  log.p=FALSE)
{
  if(any(mu <= 0))
    stop("mu must be positive")

  if(any(sigma <= 0))
    stop("sigma must be positive")

  if(any(p <= 0) || any(p >= 1))
    stop("p must be between 0 and 1")

  alpha <- mu*sigma + 1
  beta  <- sigma

  z <- qbeta(p,
             shape1=alpha,
             shape2=beta,
             lower.tail=lower.tail,
             log.p=log.p)

  z/(1-z)
}
