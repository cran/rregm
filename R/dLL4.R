dLL4 <- function(x, mu = 1, sigma = 1.5, log = FALSE)
{
    if(any(mu <= 0)) stop("mu must be positive")
    if(any(sigma <= 1)) stop("sigma must be greater than 1")
    if(any(x <= 0)) stop("x must be positive")

    lambda <- mu*(pi/sigma)/sin(pi/sigma)

    z <- (x/lambda)^sigma

    loglik <-
        log(sigma)-
        log(lambda)+
        (sigma-1)*log(x/lambda)-
        2*log1p(z)

    if(log) loglik else exp(loglik)
}
