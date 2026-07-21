LL <- function (mu.link = "log", sigma.link = "log") 
{
    mstats <- checklink("mu.link", "log-logistic", substitute(mu.link), 
                                 c("inverse", "log", "identity", "own"))
    dstats <- checklink("sigma.link", "log-logistic", substitute(sigma.link), 
                                 c("inverse", "log", "identity", "own"))

    obj <- list(
        family = c("LL", "log-logistic"),
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
        power_term <- (y / mu)^sigma
  term <- 2 * sigma * power_term / (1 + power_term)
  result <- -sigma / mu + term / mu
  return(result)
        },
        dldd = function(y, mu, sigma) {
        log_ratio <- log(y / mu)
  power_term <- (y / mu)^sigma
  term <- 2 * power_term * log_ratio / (1 + power_term)
  result <- 1 / sigma + log_ratio * (1 - 2 * power_term / (1 + power_term))
  return(result)
        },
        d2ldm2 = function(mu, sigma){-(sigma^2)/(3*mu^2)},
        d2ldd2 = function(mu, sigma){-(1 + pi^2/9)/(sigma^2)},
        d2ldmdd = function(y){rep(0, length(y))},
        G.dev.incr = function(y, mu, sigma, w, ...) {
            -2 * dLL(y, mu, sigma, log = TRUE)
        },
        rqres = expression(rqres(pfun = "pLL", type = "Continuous", y = y, mu = mu, sigma = sigma)),
        mu.initial = expression({ mu <- (y + mean(y))/2 }),
        sigma.initial = expression({ sigma <- rep(sd(y), length(y)) }),
        mu.valid = function(mu) all(mu > 0),
        sigma.valid = function(sigma) all(sigma > 0),
        y.valid = function(y) all(y > 0)
    )

    class(obj) <- c("gamlss.family", "family")
    return(obj)
}
