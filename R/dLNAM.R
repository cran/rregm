dLNAM <- function(x, mu=1, sigma=1, log=FALSE)
{
  if(any(mu <= 0))
    stop("mu must be positive")

  if(any(sigma <= 0))
    stop("sigma must be positive")

  phi <- sqrt(sigma)

  theta <- log(mu) - 0.5*phi

  dlnorm(x,
         meanlog=theta,
         sdlog=phi,
         log=log)
}
