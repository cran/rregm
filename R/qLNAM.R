qLNAM <- function(p,
                  mu=1,
                  sigma=1,
                  lower.tail=TRUE,
                  log.p=FALSE)
{
  if(any(mu <= 0))
    stop("mu must be positive")

  if(any(sigma <= 0))
    stop("sigma must be positive")

  if(any(p <= 0) || any(p >= 1))
    stop("p must be between 0 and 1")

  phi <- sqrt(sigma)

  theta <- log(mu) - 0.5*phi

  qlnorm(p,
         meanlog=theta,
         sdlog=phi,
         lower.tail=lower.tail,
         log.p=log.p)
}
