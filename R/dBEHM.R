dBEHM <-
function (x, mu = 0.5, sigma = 1, log = FALSE) 
{
    if (any(mu <= 0) | any(mu >= 1)) 
        stop(paste("mu must be between 0 and 1", "\n", ""))
    if (any(sigma <= 0)) 
        stop(paste("sigma must be positive", "\n", ""))
    a <- mu*sigma+1
    b <- (1-mu)*sigma
    fy <- dbeta(x, shape1 = a, shape2 = b, ncp = 0, log = log)
    fy
}
