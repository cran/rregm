dLL5 <- function(x, mu = 1, sigma = 1.5, log = FALSE)
{
    if (any(mu <= 0))
        stop("mu must be positive")

    if (any(sigma <= 1))
        stop("sigma must be greater than 1")

    if (any(x < 0))
        stop("x must be positive")

    A <- (x*(sigma-1)/(mu*(sigma+1)))^sigma

    log.lik <-
        log(sigma) -
        log(mu) +
        (sigma-1)*(log(x)-log(mu)) +
        sigma*(log(sigma-1)-log(sigma+1)) -
        2*log1p(A)

    if (log)
        return(log.lik)
    else
        return(exp(log.lik))
}
