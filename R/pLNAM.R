pLNAM <- function(q,
                  mu=1,
                  sigma=1,
                  lower.tail=TRUE,
                  log.p=FALSE)
{
  if(any(mu <= 0))
    stop("mu must be positive")

  if(any(sigma <= 0))
    stop("sigma must be positive")

  phi <- sqrt(sigma)

  theta <- log(mu) - 0.5*phi

  plnorm(q,
         meanlog=theta,
         sdlog=phi,
         lower.tail=lower.tail,
         log.p=log.p)
}
