LL5 <- function (mu.link = "log", sigma.link = "logshiftto1") 
{
    mstats <- checklink("mu.link", "log-logistic5", substitute(mu.link), 
                                 c("inverse", "log", "identity", "own"))
    dstats <- checklink("sigma.link", "log-logistic5", substitute(sigma.link), 
                                 c("logshiftto1","inverse", "log", "identity", "own"))
    obj <- list(
        family = c("LL5", "log-logistic5"),
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
    A <- (y*(sigma-1)/(mu*(sigma+1)))^sigma
    sigma/mu * (2*A/(1+A) - 1)
},
 dldd = function(y, mu, sigma) {
 A <- (y*(sigma-1)/(mu*(sigma+1)))^sigma
    L <- log(y) - log(mu) +
         log(sigma-1) - log(sigma+1)
    1/sigma +
    L +
    2*sigma/(sigma^2-1) -
    (2*A/(1+A))*
    (L + 2*sigma/(sigma^2-1))
},
d2ldm2 = function(mu, sigma) {
   -(sigma^2)/(3*mu^2)
},
d2ldd2 = function(y, mu, sigma) {
 A <- 1 + pi^2/9;-( A/sigma^2+4*sigma^2/(3*(sigma^2-1)^2) )
},
d2ldmdd = function(y, mu, sigma) {
  2*sigma^2/(3*mu*(sigma^2-1))
}, G.dev.incr = function(y, mu, sigma, w, ...) {
            -2 * dLL5(y, mu, sigma, log = TRUE)
        },
        rqres = expression(rqres(pfun = "pLL5", type = "Continuous", y = y, mu = mu, sigma = sigma)),
        mu.initial = expression({ mu <- (y + mean(y))/2 }),
        sigma.initial = expression({ sigma <- rep(1+sd(y), length(y)) }),
        mu.valid = function(mu) all(mu > 0),
        sigma.valid = function(sigma) all(sigma > 1),
        y.valid = function(y) all(y > 0)
    )

    class(obj) <- c("gamlss.family", "family")
    return(obj)
}

