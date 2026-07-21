pLL5 <- function(q, mu = 1, sigma = 1.5,
                 lower.tail = TRUE, log.p = FALSE)
{
    A <- (q*(sigma-1)/(mu*(sigma+1)))^sigma

    cdf <- 1 - 1/(1+A)

    if (!lower.tail)
        cdf <- 1-cdf

    if (log.p)
        cdf <- log(cdf)

    cdf
}
