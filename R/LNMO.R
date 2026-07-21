LNMO <- function(mu.link = "log",
                 sigma.link = "log")
{
  mstats <- checklink("mu.link",
                      "LNMO",
                      substitute(mu.link),
                      c("log","identity","inverse","own"))

  dstats <- checklink("sigma.link",
                      "LNMO",
                      substitute(sigma.link),
                      c("log","identity","inverse","own"))

  structure(

    list(

      family = c("LNMO","LogNormalMO"),

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
        W <- log(y)-log(mu)-sqrt(sigma)

        W/(mu*sigma)
      },
      d2ldm2 = function(y, mu, sigma)
      {
        -1/(mu^2*sigma)
      },
      dldd = function(y, mu, sigma)
      {
        W <- log(y)-log(mu)-sqrt(sigma)

        -1/(2*sigma) +
          W/(2*sigma^(3/2)) +
          W^2/(2*sigma^2)
      },
      d2ldd2 = function(y, mu, sigma)
      {
        -3/(4*sigma^2)
      },
      d2ldmdd = function(y, mu, sigma)
      {
        -1/(2*mu*sigma^(3/2))
      },
      G.dev.incr = function(y, mu, sigma, w, ...)
      {
        -2*dLNMO(y,
                 mu = mu,
                 sigma = sigma,
                 log = TRUE)
      },
      rqres = expression(
        rqres(
          pfun = "pLNMO",
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
        exp(log(mu)+sqrt(sigma)-sigma/2)
      },

      variance = function(mu, sigma)
      {
        m1 <- exp(log(mu)+sqrt(sigma)-sigma/2)

        (exp(sigma)-1)*m1^2
      }

    ),

    class = c("gamlss.family","family")
  )
}
