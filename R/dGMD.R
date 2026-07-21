dGMD <- function(x, mu = 1, sigma = 1, log = FALSE)
{
  if(any(mu <= 0))
    stop(paste("mu must be positive","\n",""))
  
  if(any(sigma <= 0))
    stop(paste("sigma must be positive","\n",""))
  
  A <- (sqrt(sigma*(sigma+4/3)) + sigma)/2
  
  alpha <- A + 1/3
  beta  <- A/mu
  
  fy <- dgamma(x,
               shape = alpha,
               rate  = beta,
               log   = log)
  
  fy
}
