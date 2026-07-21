dBPAM <- function(x, mu = 1, sigma = 1.5, log = FALSE)
{
  if(any(mu <= 0))
    stop(paste("mu must be positive","\n",""))

  if(any(sigma <= 0))
    stop(paste("sigma must be positive","\n",""))

  alpha <- mu*sigma
  beta  <- sigma + 1

  logfy <-
    (alpha-1)*log(x) -
    (alpha+beta)*log1p(x) -
    lbeta(alpha,beta)

  if(log)
    return(logfy)
  else
    return(exp(logfy))
}