pLL4 <- function(q, mu=1, sigma=1.5,
                 lower.tail=TRUE,
                 log.p=FALSE)
{

    lambda <- mu*(pi/sigma)/sin(pi/sigma)

    z <- (q/lambda)^sigma

    cdf <- z/(1+z)

    if(!lower.tail)
        cdf <- 1-cdf

    if(log.p)
        cdf <- log(cdf)

    cdf
}
