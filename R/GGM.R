GGM <- function(mu.link = "log", sigma.link = "log")
{
  
  mstats <- checklink("mu.link", "GGM",
                      substitute(mu.link),
                      c("log", "identity", "inverse", "own"))
  
  dstats <- checklink("sigma.link", "GGM",
                      substitute(sigma.link),
                      c("log", "identity", "inverse", "own"))
  
  structure(
    
    list(
      
      family = c("GGM", "GammaGM"),
      
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
        A <- (sqrt(sigma*(sigma+2)) + sigma)/2
        alpha <- A + 0.5
        
        -alpha/mu + A*y/mu^2
      },
      
      d2ldm2 = function(mu, sigma)
{
  A <- (sqrt(sigma*(sigma+2)) + sigma)/2

  alpha <- A + 0.5

  -alpha/mu^2
},
      
      dldd = function(y, mu, sigma)
      {
        A <- (sqrt(sigma*(sigma+2)) + sigma)/2
        
        Ap <- 0.5*(1 + (sigma+1)/sqrt(sigma*(sigma+2)))
        
        alpha <- A + 0.5
        
        Ap*(log(A/mu) +
              alpha/A -
              digamma(alpha) +
              log(y) -
              y/mu)
      },

      d2ldd2 = function(sigma)
      {
        A <- (sqrt(sigma*(sigma+2)) + sigma)/2
        
        Ap <- 0.5*(1 + (sigma+1)/sqrt(sigma*(sigma+2)))
        
        alpha <- A + 0.5
        
        Ap^2*(1/A -
                1/(2*A^2) -
                trigamma(alpha))
      },
      
      d2ldmdd = function(mu, sigma)
      {
        A <- (sqrt(sigma*(sigma+2)) + sigma)/2
        
        Ap <- 0.5*(1 + (sigma+1)/sqrt(sigma*(sigma+2)))
        
        Ap/(2*A*mu)
      },
      
      G.dev.incr = function(y, mu, sigma, w, ...)
      {
        -2*dGGM(y,
                mu = mu,
                sigma = sigma,
                log = TRUE)
      },
      
      rqres = expression(
        rqres(pfun = "pGGM",
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
        A <- (sqrt(sigma*(sigma+2)) + sigma)/2
        alpha <- A + 0.5
        
        alpha*mu^2/A^2
      }
      
    ),
    
    class = c("gamlss.family", "family")
    
  )
}

