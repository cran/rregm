rLL2 <- function (n, mu = 1, sigma = 1) 
{
    if (any(mu <= 0)) 
        stop(paste("mu must be positive", "\n", ""))
    if (any(sigma <= 0)) 
        stop(paste("sigma must be positive", "\n", ""))
    if (any(n <= 0)) 
        stop(paste("n must be a positive integer", "\n", ""))
    n <- ceiling(n)
    p <- runif(n)
    qLL2(p, mu = mu, sigma = sigma)
}
