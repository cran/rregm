qLL2 <- function (p, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE) 
{
if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
    if (any(mu <= 0)) 
        stop(paste("mu must be positive", "\n", ""))
    if (any(sigma <= 0)) 
        stop(paste("sigma must be positive", "\n", ""))
    if (any(p < 0) | any(p > 1)) 
        stop(paste("p must be between 0 and 1", "\n", ""))
  t <- ((p / (1 - p)) / mu)^(1 / sigma)
  return(t)
}
