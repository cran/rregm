qGMO <- function(p,
                 mu = 1,
                 sigma = 1,
                 lower.tail = TRUE,
                 log.p = FALSE)
{
  if(any(mu <= 0))
    stop(paste("mu must be positive","\n",""))
  
  if(any(sigma <= 0))
    stop(paste("sigma must be positive","\n",""))
  
  if(any(p <= 0) | any(p >= 1))
    stop(paste("p must be between 0 and 1","\n",""))
  
  A <- (sqrt(sigma*(sigma+4)) + sigma)/2
  
  alpha <- A + 1
  beta  <- A/mu
  
  qgamma(p,
         shape = alpha,
         rate = beta,
         lower.tail = lower.tail,
         log.p = log.p)
}
