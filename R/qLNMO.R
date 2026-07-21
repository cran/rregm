qLNMO <- function(p,
                  mu=1,
                  sigma=1,
                  lower.tail=TRUE,
                  log.p=FALSE)
{
  if(any(mu <= 0))
    stop("mu must be positive")

  if(any(sigma <= 0))
    stop("sigma must be positive")

  qlnorm(p,
         meanlog = log(mu)+sigma,
         sdlog   = sqrt(sigma),
         lower.tail = lower.tail,
         log.p = log.p)
}
