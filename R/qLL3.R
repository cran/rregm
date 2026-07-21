qLL3 <- function(p, mu = 1, sigma = 1.5, lower.tail = TRUE, log.p = FALSE) 
{
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p

  if (any(mu <= 0)) 
      stop("mu must be positive")
  if (any(sigma <= 1)) 
      stop("sigma must be greater than 1")
  if (any(p < 0) | any(p > 1)) 
      stop("p must be between 0 and 1")

  c <- pi / (sigma * sin(pi / sigma))
  t <- mu * (p / (1 - p))^(1 / sigma) / c
  return(t)
}
