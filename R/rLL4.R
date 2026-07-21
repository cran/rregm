rLL4 <- function(n, mu=1, sigma=1.5)
{
    if (any(mu <= 0)) 
        stop(paste("mu must be positive", "\n", ""))
    if (any(sigma <= 1)) 
        stop("sigma must be greater than 1")
    if (any(n <= 0)) 
        stop(paste("n must be a positive integer", "\n", ""))
    n <- ceiling(n)
    p <- runif(n)
    qLL4(p,mu,sigma)
}

