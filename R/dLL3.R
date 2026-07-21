dLL3 <- function (x, mu = 1, sigma = 1.5, log = FALSE) 
{
    if (any(mu <= 0)) 
        stop("mu must be positive")
    if (any(sigma <= 1)) 
        stop("sigma must be greater than 1")
    if (any(x < 0)) 
        stop("x must be positive")
    c <- pi / (sigma * sin(pi / sigma))
    A <- (c * x / mu)^sigma
    log.lik <- log(sigma) + sigma * log(c) - sigma * log(mu) + (sigma - 1) * log(x) - 2 * log1p(A)    
    if (log == FALSE) 
        fy <- exp(log.lik)
    else 
        fy <- log.lik
    fy
}
