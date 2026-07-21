BPGM <- function(mu.link = "log",
                 sigma.link = "log")
{
  mstats <- checklink("mu.link",
                      "BPGM",
                      substitute(mu.link),
                      c("log","identity","inverse","own"))

  dstats <- checklink("sigma.link",
                      "BPGM",
                      substitute(sigma.link),
                      c("log","identity","inverse","own"))

  structure(

    list(

      family = c("BPGM","BetaPrimeGM"),

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

      dldm = function(y, mu, sigma)
      {
        alpha <- mu*sigma + 1/2
        beta  <- sigma + 1/2

        sigma*(log(y)-log1p(y)-digamma(alpha)+digamma(alpha+beta))
      },

      d2ldm2 = function(y, mu, sigma)
      {
        alpha <- mu*sigma + 1/2
        beta  <- sigma + 1/2

        -sigma^2*(trigamma(alpha)-trigamma(alpha+beta))
      },

      dldd = function(y, mu, sigma)
      {
        alpha <- mu*sigma + 1/2
        beta  <- sigma + 1/2

        mu*log(y)-(1+mu)*log1p(y)-mu*digamma(alpha)-digamma(beta)+
          (1+mu)*digamma(alpha+beta)
      },

      d2ldd2 = function(y, mu, sigma)
      {
        alpha <- mu*sigma + 1/2
        beta  <- sigma + 1/2

        -mu^2*trigamma(alpha)-trigamma(beta)+
          (1+mu)^2*trigamma(alpha+beta)
      },

      d2ldmdd = function(y, mu, sigma)
      {
        alpha <- mu*sigma + 1/2
        beta  <- sigma + 1/2

        sigma*(-mu*trigamma(alpha)+(1+mu)*trigamma(alpha+beta))
      },

      G.dev.incr = function(y, mu, sigma, w, ...)
      {
        -2*dBPGM(y,
                 mu=mu,
                 sigma=sigma,
                 log=TRUE)
      },

      rqres = expression(
        rqres(
          pfun="pBPGM",
          type="Continuous",
          y=y,
          mu=mu,
          sigma=sigma
        )
      ),

      mu.initial = expression({
        mu <- rep(exp(mean(log(y))), length(y))
      }),

      sigma.initial = expression({
        sigma <- rep(sd(log(y)), length(y))
      }),

      mu.valid = function(mu)
        all(mu > 0),

      sigma.valid = function(sigma)
        all(sigma > 0),

      y.valid = function(y)
        all(y > 0),

      mean = function(mu, sigma)
      {
        mu
      },

      variance = function(mu, sigma)
      {
        rep(NA,length(mu))
      }

    ),

    class = c("gamlss.family","family")
  )
}

