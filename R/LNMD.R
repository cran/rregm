LNMD <- function(mu.link = "log",
                 sigma.link = "log")
{
  mstats <- checklink("mu.link",
                      "LNMD",
                      substitute(mu.link),
                      c("log","identity","inverse","own"))

  dstats <- checklink("sigma.link",
                      "LNMD",
                      substitute(sigma.link),
                      c("log","identity","inverse","own"))

  structure(

    list(

      family = c("LNMD","LogNormalMD"),

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
        z <- log(y)-log(mu)

        z/(mu*sigma)
      },
      d2ldm2 = function(y, mu, sigma)
      {
        -1/(mu^2*sigma)
      },
      dldd = function(y, mu, sigma)
      {
        z <- log(y)-log(mu)

        -1/(2*sigma) +
          z^2/(2*sigma^2)
      },
      d2ldd2 = function(y, mu, sigma)
      {
        -1/(2*sigma^2)
      },
      d2ldmdd = function(y, mu, sigma)
      {
        0
      },
      G.dev.incr = function(y, mu, sigma, w, ...)
      {
        -2*dLNMD(y,
                 mu = mu,
                 sigma = sigma,
                 log = TRUE)
      },
      rqres = expression(
        rqres(
          pfun = "pLNMD",
          type = "Continuous",
          y = y,
          mu = mu,
          sigma = sigma
        )
      ),
      mu.initial = expression({
        mu <- rep(exp(mean(log(y))), length(y))
      }),

      sigma.initial = expression({
        sigma <- rep(var(log(y)), length(y))
      }),
      mu.valid = function(mu)
        all(mu > 0),

      sigma.valid = function(sigma)
        all(sigma > 0),

      y.valid = function(y)
        all(y > 0),
      mean = function(mu, sigma)
      {
        exp(log(mu)+sigma/2)
      },

      variance = function(mu, sigma)
      {
        (exp(sigma)-1)*exp(2*log(mu)+sigma)
      }

    ),

    class = c("gamlss.family","family")
  )
}
