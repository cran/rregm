IGMO <-
function (mu.link = "log", sigma.link = "logshiftto1") 
{
  mstats <- checklink("mu.link", "Gamma", substitute(mu.link), 
                      c("inverse", "log", "identity", "own"))
  dstats <- checklink("sigma.link", "Gamma", substitute(sigma.link), 
                      c("logshiftto1","inverse", "log", "identity", "own"))
  structure(list(family = c("IGMO", "Inverse_gamma_mode"), 
                 parameters = list(mu = TRUE,  sigma = TRUE), 
                 nopar         = 2, 
                 type = "Continuous", 
                 mu.link       = as.character(substitute(mu.link)), 
                 sigma.link    = as.character(substitute(sigma.link)), 
                 mu.linkfun    = mstats$linkfun, 
                 sigma.linkfun = dstats$linkfun, 
                 mu.linkinv    = mstats$linkinv, 
                 sigma.linkinv = dstats$linkinv, 
                 mu.dr         = mstats$mu.eta, 
                 sigma.dr      = dstats$mu.eta, 
                 dldm    = function(y, mu, sigma){(sigma-1)/mu - sigma/y}, 
                 dldd    = function(y, mu, sigma){log(sigma*mu) + (sigma -1 )/sigma - digamma(sigma -1)-log(y)- mu/y  },
                 d2ldm2  = function(mu, sigma){- (sigma - 1 )/mu^2},
                 d2ldd2  = function(sigma){ 1/sigma + 1/(sigma^2) - trigamma(sigma-1)},
                 d2ldmdd = function(y,mu){ 1/mu - 1/y},
                 G.dev.incr = function(y, mu, sigma, w, 
                                       ...) -2 * dIGMO(y, mu, sigma, log = TRUE), rqres = expression(rqres(pfun = "pIGMO", 
                                       type = "Continuous", y = y, mu = mu, sigma = sigma)), 
                 mu.initial = expression({
                   mu <- (y + mean(y))/2
                 }), 
                 sigma.initial = expression({
                   sigma <- rep(1.5, length(y))
                 }), 
                 mu.valid    = function(mu) all(mu > 0), 
                 sigma.valid = function(sigma) all(sigma > 1), 
                 y.valid     = function(y) all(y > 0), 
                 mean        = function(mu,  sigma) (sigma*mu) /(sigma -2) , 
                 variance    = function(mu, sigma) ((sigma^2)*(mu^2)) /((sigma -3)*((sigma - 2)^2))
                 ), class = c("gamlss.family", "family")
            )
}
