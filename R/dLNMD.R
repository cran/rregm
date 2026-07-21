dLNMD <- function(x, mu=1, sigma=1, log=FALSE)
{
  if(any(mu <= 0))
    stop("mu must be positive")

  if(any(sigma <= 0))
    stop("sigma must be positive")

  dlnorm(x,
         meanlog=log(mu),
         sdlog=sqrt(sigma),
         log=log)
}
