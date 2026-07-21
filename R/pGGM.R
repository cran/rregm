pGGM <- function(q,
                 mu = 1,
                 sigma = 1,
                 lower.tail = TRUE,
                 log.p = FALSE)
{
  if(any(mu <= 0))
    stop(paste("mu must be positive","\n",""))
  
  if(any(sigma <= 0))
    stop(paste("sigma must be positive","\n",""))
  
  A <- (sqrt(sigma*(sigma+2)) + sigma)/2
  
  alpha <- A + 0.5
  beta  <- A/mu
  
  cdf <- pgamma(q,
                shape = alpha,
                rate  = beta,
                lower.tail = lower.tail,
                log.p = log.p)
  
  cdf
}
