LL3 <- function (mu.link = "log", sigma.link = "logshiftto1") 
{
    mstats <- checklink("mu.link", "log-logistic3", substitute(mu.link), 
                                 c("inverse", "log", "identity", "own"))
    dstats <- checklink("sigma.link", "log-logistic3", substitute(sigma.link), 
                                 c("logshiftto1","inverse", "log", "identity", "own"))
cot <- function(x) 1/tan(x)
    obj <- list(
        family = c("LL3", "log-logistic3"),
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
  		A <- ((pi*y)/(mu*sigma*sin(pi/sigma)))^sigma
 sigma/mu*(2*A/(1+A)-1)
	},dldd = function(y, mu, sigma) {
  S <- sin(pi / sigma)
  Z <- (pi * y) / (mu * sigma * S)
  Zs <- Z^sigma
  SS <- (sigma * S)^2
  L <- log(pi) + log(y) - log(mu) - log(sigma) - log(S)
  dS <- S - pi * cos(pi / sigma) / sigma
  num <- (L * Zs - pi * sigma * y * Z^(sigma - 1) * dS / (mu * SS))
  den <- Zs + 1
  term <- 2 * (num / den) + log(mu) + log(sigma) + log(S) + sigma^2 * S * dS / SS
  1 / sigma + log(pi) + log(y) - term
},d2ldm2 = function(mu,sigma){-(sigma^2)/(3*mu^2)}, 
d2ldd2 = function(mu, sigma){A <- 1 - (pi/sigma) * cot(pi/sigma);-1/sigma^2-A^2/3-(2/(3*sigma^2))*(pi^2/6 - 1)}, 
d2ldmdd = function(mu, sigma){-(sigma - pi*cot(pi/sigma))/(3*mu)},
        G.dev.incr = function(y, mu, sigma, w, ...) {
            -2 * dLL3(y, mu, sigma, log = TRUE)
        },
        rqres = expression(rqres(pfun = "pLL3", type = "Continuous", y = y, mu = mu, sigma = sigma)),
        mu.initial = expression({ mu <- (y + mean(y))/2 }),
        sigma.initial = expression({ sigma <- rep(1+sd(y), length(y)) }),
        mu.valid = function(mu) all(mu > 0),
        sigma.valid = function(sigma) all(sigma > 1),
        y.valid = function(y) all(y > 0)
    )

    class(obj) <- c("gamlss.family", "family")
    return(obj)
}
