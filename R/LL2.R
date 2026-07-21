LL2 <- function (mu.link = "log", sigma.link = "log") 
{
    mstats <- checklink("mu.link", "log-logistic2", substitute(mu.link), 
                                 c("inverse", "log", "identity", "own"))
    dstats <- checklink("sigma.link", "log-logistic2", substitute(sigma.link), 
                                 c("inverse", "log", "identity", "own"))

    obj <- list(
        family = c("LL2", "log-logistic2"),
        parameters = list(mu = TRUE, sigma = TRUE),
        nopar = 2,
        type = "Continuous",
        mu.link = as.character(substitute(mu.link)),
        sigma.link = as.character(substitute(sigma.link)),
        mu.linkfun = mstats$linkfun,
        sigma.linkfun = dstats$linkfun,
        mu.linkinv = mstats$linkinv,
        sigma.linkinv = dstats$linkinv,
        mu.dr = mstats$mu.eta,
        sigma.dr = dstats$mu.eta,
        dldm = function(y, mu, sigma) {
            (1 / mu) - (2 * y^sigma) / (1 + mu * y^sigma)
        },
        dldd = function(y, mu, sigma) {
            (1 / sigma) + log(y) - (2 * mu * y^sigma * log(y)) / (1 + mu * y^sigma)
        },
        d2ldm2 = function(mu,sigma){-1/(3*mu^2)},
        d2ldd2 = function(mu, sigma){-(3+pi^2+3*log(mu)^2)/(9*sigma^2)},
        d2ldmdd = function(mu, sigma){(log(mu))/(3*mu*sigma)},
        G.dev.incr = function(y, mu, sigma, w, ...) {
            -2 * dLL2(y, mu, sigma, log = TRUE)
        },
        rqres = expression(rqres(pfun = "pLL2", type = "Continuous", y = y, mu = mu, sigma = sigma)),
        mu.initial = expression({ mu <- (y + mean(y))/2 }),
        sigma.initial = expression({ sigma <- rep(1, length(y)) }),
        mu.valid = function(mu) all(mu > 0),
        sigma.valid = function(sigma) all(sigma > 0),
        y.valid = function(y) all(y > 0)
    )

    class(obj) <- c("gamlss.family", "family")
    return(obj)
}

