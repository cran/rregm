pLL2 <- function (q, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE) 
{
    if (any(mu <= 0)) 
        stop(paste("mu must be positive", "\n", ""))
    if (any(sigma <= 0)) 
        stop(paste("sigma must be positive", "\n", ""))
    if (any(q < 0)) 
        stop(paste("y must be positive", "\n", ""))
  cdf <- (mu * q^sigma) / (1 + mu * q^sigma)
  if (!lower.tail) cdf <- 1 - cdf
  if (log.p) cdf <- log(cdf)
  cdf
}
