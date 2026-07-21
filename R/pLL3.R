pLL3 <- function(q, mu = 1, sigma = 1.5, lower.tail = TRUE, log.p = FALSE) 
{
    if (any(mu <= 0)) 
        stop("mu must be positive")
    if (any(sigma <= 1)) 
        stop("sigma must be greater than 1")
    if (any(q < 0)) 
        stop("q must be positive")

    c <- pi / (sigma * sin(pi / sigma))
    A <- (c * q / mu)^sigma
    cdf <- 1-1 / (1 + A)

    if (!lower.tail) cdf <- 1 - cdf
    if (log.p) cdf <- log(cdf)

    return(cdf)
}
