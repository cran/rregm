dBPMO <- function(x, mu=1, sigma=1.5, log=FALSE)
{
  if(any(mu <= 0))
    stop("mu must be positive")

  if(any(sigma <= 0))
    stop("sigma must be positive")

  alpha <- mu*(sigma+1) + 1
  beta  <- sigma

  logf <- dbeta(x/(1+x),
                shape1=alpha,
                shape2=beta,
                log=TRUE) -
    2*log1p(x)

  if(log) logf else exp(logf)
}
