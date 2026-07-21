qLL4 <- function(p, mu=1, sigma=1.5,
                 lower.tail=TRUE,
                 log.p=FALSE)
{

    if(log.p)
        p <- exp(p)

    if(!lower.tail)
        p <- 1-p

    lambda <- mu*(pi/sigma)/sin(pi/sigma)

    lambda*(p/(1-p))^(1/sigma)
}
