rBEAM <-
function (n, mu = 0.5, sigma = 1) 
{
    if (any(mu <= 0) | any(mu >= 1)) 
        stop(paste("mu must be between 0 and 1", "\n", ""))
    if (any(sigma <= 0)) 
        stop(paste("sigma must be positive", "\n", ""))
    if (any(n <= 0)) 
        stop(paste("n must be a positive integer", "\n", ""))
    n <- ceiling(n)
    p <- runif(n)
    a <- mu*sigma
    b <- (1-mu)*sigma
    r <- qbeta(p, shape1 = a, shape2 = b)
    r
}
