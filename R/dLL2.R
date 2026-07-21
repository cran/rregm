dLL2 <- function (x, mu = 1, sigma = 1, log = FALSE) 
{
    if (any(mu <= 0)) 
        stop(paste("mu must be positive", "\n", ""))
    if (any(sigma <= 0)) 
        stop(paste("sigma must be positive", "\n", ""))
    if (any(x < 0)) 
        stop(paste("x must be positive", "\n", ""))
    log.lik <- log(mu) + log(sigma) + (sigma - 1) * log(x) - 2 * log(1 + mu * x^sigma)
    if (log == FALSE) 
        fy <- exp(log.lik)
    else fy <- log.lik
    fy
}
