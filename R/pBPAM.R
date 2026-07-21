pBPAM <- function(q,
                  mu = 1,
                  sigma = 1.5,
                  lower.tail = TRUE,
                  log.p = FALSE)
{
  if(any(mu <= 0))
    stop(paste("mu must be positive","\n",""))

  if(any(sigma <= 0))
    stop(paste("sigma must be positive","\n",""))

  alpha <- mu*sigma
  beta  <- sigma + 1

  z <- q/(1+q)

  pbeta(z,
        shape1 = alpha,
        shape2 = beta,
        lower.tail = lower.tail,
        log.p = log.p)
}
