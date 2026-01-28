IGHM <-
function (mu.link = "log", sigma.link = "log") 
{
    mstats <- checklink("mu.link", "Gamma", substitute(mu.link), 
        c("inverse", "log", "identity", "own"))
    dstats <- checklink("sigma.link", "Gamma", substitute(sigma.link), 
        c("inverse", "log", "identity", "own"))
    structure(list(family = c("IGHM", "Inverse_gamma_harmonic"), 
                   parameters    = list(mu = TRUE,sigma = TRUE), 
                   nopar         = 2, 
                   type = "Continuous", 
                   mu.link       = as.character(substitute(mu.link)), 
                   sigma.link    = as.character(substitute(sigma.link)), mu.linkfun = mstats$linkfun, 
                   sigma.linkfun = dstats$linkfun, mu.linkinv = mstats$linkinv, 
                   sigma.linkinv = dstats$linkinv, mu.dr = mstats$mu.eta, 
                   sigma.dr      = dstats$mu.eta, 
                   dldm   = function(y, mu, sigma){sigma/mu - sigma/y}, 
                 dldd   = function(y, mu, sigma){log(sigma*mu) + 1 - digamma(sigma)-log(y)- mu/y  },
                 d2ldm2 = function(mu, sigma){- sigma/mu^2},
                 d2ldd2 = function(sigma){ 1/sigma - trigamma(sigma)},
                d2ldmdd = function(y,mu){ 1/mu - 1/y},
                G.dev.incr = function(y, mu, sigma, w, 
                                      ...) -2 * dIGHM(y, mu, sigma, log = TRUE), rqres = expression(rqres(pfun = "pIGHM", 
                                       type = "Continuous", y = y, mu = mu, sigma = sigma)), 
                mu.initial = expression({
                               mu <- (y + mean(y))/2
                               }), 
                sigma.initial = expression({
                          sigma <- rep(1, length(y))
                               }), 
                mu.valid    = function(mu) all(mu > 0), 
                sigma.valid = function(sigma) all(sigma >  0), 
                y.valid     = function(y) all(y > 0), 
                mean        = function(mu, sigma) mu*sigma/(sigma-1), 
                variance    = function(mu, sigma) (sigma^2 * mu^2)/((sigma-2)*(sigma-1)^2)
                ), class = c("gamlss.family", "family")
              )
}
