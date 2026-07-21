rGHM <- function(n,
                 mu = 1,
                 sigma = 1)
{
  if(any(mu <= 0))
    stop(paste("mu must be positive","\n",""))
  
  if(any(sigma <= 0))
    stop(paste("sigma must be positive","\n",""))
  
  if(any(n <= 0))
    stop(paste("n must be a positive integer","\n",""))
  
  n <- ceiling(n)
  
  p <- runif(n)
  
  A <- (sqrt(sigma*(sigma+4)) + sigma)/2
  
  alpha <- A + 1
  beta  <- A/mu
  
  r <- qgamma(p,
              shape = alpha,
              rate = beta)
  
  r
}


