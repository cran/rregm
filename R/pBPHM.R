pBPHM <- function(q,
                  mu=1,
                  sigma=1.5,
                  lower.tail=TRUE,
                  log.p=FALSE)
{
  if(any(mu <= 0))
    stop("mu must be positive")

  if(any(sigma <= 0))
    stop("sigma must be positive")

  alpha <- mu*sigma + 1
  beta  <- sigma

  pbeta(q/(1+q),
        shape1=alpha,
        shape2=beta,
        lower.tail=lower.tail,
        log.p=log.p)
}
