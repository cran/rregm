GAM <- function(mu.link = "log", sigma.link = "log")
{
  mstats <- checklink("mu.link", "GAM",
                      substitute(mu.link),
                      c("log", "identity", "inverse", "own"))
  dstats <- checklink("sigma.link", "GAM",
                      substitute(sigma.link),
                      c("log", "identity", "inverse", "own"))
  structure(

    list(
 
      family = c("GAM", "GammaAM"),
      
      parameters = list(mu = TRUE,
                        sigma = TRUE),
      
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
      
      dldm = function(y, mu, sigma)
      {
        sigma*(y - mu)/mu^2
      },
      
      d2ldm2 = function(mu, sigma)
      {
        -sigma/mu^2
      },
      
      dldd = function(y, mu, sigma)
      {
        log(sigma/mu) +
          1 -
          digamma(sigma) +
          log(y) -
          y/mu
      },
      
      d2ldd2 = function(sigma)
      {
        1/sigma - trigamma(sigma)
      },
      
      d2ldmdd = function(mu)
      {
        rep(0, length(mu))
      },
      
      G.dev.incr = function(y, mu, sigma, w, ...)
      {
        -2*dGAM(y,
                mu = mu,
                sigma = sigma,
                log = TRUE)
      },
      
      rqres = expression(
        rqres(pfun = "pGAM",
              type = "Continuous",
              y = y,
              mu = mu,
              sigma = sigma)
      ),
      
      mu.initial = expression(
      {
        mu <- rep(mean(y), length(y))
      }),
      
      sigma.initial = expression(
      {
        s <- sd(y)
        
        if(is.na(s) || s <= 0)
          s <- 1
        
        sigma <- rep(s, length(y))
      }),      
      mu.valid = function(mu)
      {
        all(mu > 0)
      },
      
      sigma.valid = function(sigma)
      {
        all(sigma > 0)
      },
      
      y.valid = function(y)
      {
        all(y > 0)
      },
      
      mean = function(mu, sigma)
      {
        mu
      },
      
      variance = function(mu, sigma)
      {
        mu^2/sigma
      }
      
    ),
    
    class = c("gamlss.family", "family")
    
  )
}









