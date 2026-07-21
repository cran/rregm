dLL <- function (x, mu = 1, sigma = 1, log = FALSE) 
{
    if (any(mu <= 0)) 
        stop(paste("mu must be positive", "\n", ""))
    if (any(sigma <= 0)) 
        stop(paste("sigma must be positive", "\n", ""))
    if (any(x < 0)) 
        stop(paste("x must be positive", "\n", ""))
A <- (x /mu)^sigma
  log.lik <- log(sigma) - log(mu) + (sigma - 1) * (log(x) - log(mu)) - 2 * log1p(A)
if (log == FALSE) 
        fy <- exp(log.lik)
    else fy <- log.lik
    fy
}
