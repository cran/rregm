qBEAM <-
function (p, mu = 0.5, sigma = 1, lower.tail = TRUE, log.p = FALSE) 
{
    if (any(mu <= 0) | any(mu >= 1)) 
        stop(paste("mu must be between 0 and 1", "\n", ""))
    if (any(sigma <= 0)) 
        stop(paste("sigma must be positive", "\n", ""))
    if (any(p <= 0) | any(p >= 1)) 
        stop(paste("p must be between 0 and 1", "\n", ""))
    a <- mu*sigma
    b <- (1-mu)*sigma
    q <- qbeta(p, shape1 = a, shape2 = b, lower.tail = lower.tail, 
        log.p = log.p)
    q
}
