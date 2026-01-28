pBEHM <-
function (q, mu = 0.5, sigma = 1, lower.tail = TRUE, log.p = FALSE) 
{
    if (any(mu <= 0) | any(mu >= 1)) 
        stop(paste("mu must be between 0 and 1", "\n", ""))
    if (any(sigma <= 0)) 
        stop(paste("sigma must be positive", "\n", ""))
    a <- mu*sigma+1
    b <- (1-mu)*sigma
    cdf <- pbeta(q, shape1 = a, shape2 = b, ncp = 0, lower.tail = lower.tail, 
        log.p = log.p)
    cdf
}
