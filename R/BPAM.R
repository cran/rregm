BPAM <- function(mu.link = "log",
                 sigma.link = "log")
{
  mstats <- checklink("mu.link",
                      "BPAM",
                      substitute(mu.link),
                      c("log","identity","inverse","own"))

  dstats <- checklink("sigma.link",
                      "BPAM",
                      substitute(sigma.link),
                      c("log","identity","inverse","own"))

  structure(

    list(

      family = c("BPAM","BetaPrimeAM"),

      parameters = list(
        mu = TRUE,
        sigma = TRUE
      ),

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

      #################################################
      # score mu
      #################################################

      dldm = function(y, mu, sigma)
      {
        alpha <- mu*sigma
        beta  <- sigma + 1

        sigma*(log(y)-log1p(y)-digamma(alpha)+digamma(alpha+beta))
      },

      #################################################
      # Fisher mu-mu
      #################################################

      d2ldm2 = function(y, mu, sigma)
{
  alpha <- mu*sigma
  beta  <- sigma + 1

  -sigma^2*(trigamma(alpha)-trigamma(alpha+beta))
},

      #################################################
      # score sigma
      #################################################

      dldd = function(y, mu, sigma)
      {
        alpha <- mu*sigma
        beta  <- sigma + 1

        mu*log(y)-(1+mu)*log1p(y)-mu*digamma(alpha)-digamma(beta)+
        (1+mu)*digamma(alpha+beta)
      },

      #################################################
      # Fisher sigma-sigma
      #################################################

      d2ldd2 = function(y, mu, sigma)
{
  alpha <- mu*sigma
  beta  <- sigma + 1

  -mu^2*trigamma(alpha)-trigamma(beta)+
  (1+mu)^2*trigamma(alpha+beta)
},

      #################################################
      # Fisher mu-sigma
      #################################################

      d2ldmdd = function(y, mu, sigma)
{
  alpha <- mu*sigma
  beta  <- sigma + 1

  sigma*(-mu*trigamma(alpha)+(1+mu)*trigamma(alpha+beta))
},

      #################################################
      # deviance
      #################################################

      G.dev.incr = function(y, mu, sigma, w, ...)
      {
        -2*dBPAM(y,
                 mu = mu,
                 sigma = sigma,
                 log = TRUE)
      },

      #################################################
      # residuals
      #################################################

      rqres = expression(
        rqres(
          pfun = "pBPAM",
          type = "Continuous",
          y = y,
          mu = mu,
          sigma = sigma
        )
      ),

      #################################################
      # initial values
      #################################################

      mu.initial = expression({
    mu <- rep(exp(mean(log(y))), length(y))
}),

      sigma.initial = expression({
    sigma <- rep(sd(log(y)), length(y))
}),

      #################################################
      # validity
      #################################################

      mu.valid = function(mu)
        all(mu > 0),

      sigma.valid = function(sigma)
        all(sigma > 0),

      y.valid = function(y)
        all(y > 0),

      #################################################
      # moments
      #################################################

      mean = function(mu, sigma)
      {
        mu
      },

      variance = function(mu, sigma)
      {
        rep(NA, length(mu))
      }

    ),

    class = c("gamlss.family","family")
  )
}

