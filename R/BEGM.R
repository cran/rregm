BEGM <-
function (mu.link = "logit", sigma.link = "log") 
{
    mstats <- checklink("mu.link", "BetaGM", substitute(mu.link), 
        c("logit", "probit", "cloglog", "cauchit", "log", "own"))
    dstats <- checklink("sigma.link", "BetaGM", substitute(sigma.link), 
        c("log", "inverse", "identity", "own"))
    structure(list(family = c("BEGM", "BetaGM"), parameters = list(mu = TRUE, 
        sigma = TRUE), nopar = 2, type = "Continuous", mu.link = as.character(substitute(mu.link)), 
        sigma.link = as.character(substitute(sigma.link)), mu.linkfun = mstats$linkfun, 
        sigma.linkfun = dstats$linkfun, mu.linkinv = mstats$linkinv, 
        sigma.linkinv = dstats$linkinv, mu.dr = mstats$mu.eta, 
        sigma.dr = dstats$mu.eta, dldm = function(y, mu, sigma) {
c1<-0.5; c2<-0.5
            a <- mu*sigma+c1
            b <- (1-mu)*sigma+c2-c1
            dldm <- sigma*(log(y)-log1p(-y)-digamma(a)+digamma(b))
            dldm
        }, d2ldm2 = function(mu, sigma) {
c1<-0.5; c2<-0.5
            a <- mu*sigma+c1
            b <- (1-mu)*sigma+c2-c1
            d2ldm2 <- -sigma^2*(trigamma(a)+trigamma(b))
            d2ldm2
        }, dldd = function(y, mu, sigma) {
c1<-0.5; c2<-0.5
            a <- mu*sigma+c1
            b <- (1-mu)*sigma+c2-c1
            dldd <- mu*(log(y)-log1p(-y)-digamma(a)+digamma(b))+log1p(-y)-digamma(b)+digamma(a+b)
            dldd
        }, d2ldd2 = function(mu, sigma) {
c1<-0.5; c2<-0.5
            a <- mu*sigma+c1
            b <- (1-mu)*sigma+c2-c1
            d2ldd2 <- -mu^2*(trigamma(a)+trigamma(b))+trigamma(b)*(2*mu-1)+trigamma(a+b)
            d2ldd2
        }, d2ldmdd = function(mu, sigma) {
c1<-0.5; c2<-0.5
            a <- mu*sigma+c1
            b <- (1-mu)*sigma+c2-c1
            d2ldmdd <- log(y)-log1p(-y)-digamma(a)+digamma(b)+sigma*(-trigamma(a)*mu+trigamma(b)*(1-mu))
            d2ldmdd
        }, G.dev.incr = function(y, mu, sigma, w, ...) -2 * dBEGM(y, 
            mu, sigma, log = TRUE), rqres = expression(rqres(pfun = "pBEGM", 
            type = "Continuous", y = y, mu = mu, sigma = sigma)), 
        mu.initial = expression({mu <- (y + mean(y))/2}), 
sigma.initial = expression({sigma <- rep(1, length(y))}), 
mu.valid = function(mu) all(mu > 0 & mu < 1), 
sigma.valid = function(sigma) all(sigma > 0), 
y.valid = function(y) all(y > 0 & y < 1), 
mean = function(mu, sigma){c1<-0.5; c2<-0.5;(mu*sigma+c1)/(sigma+c1+c2)}, 
variance = function(mu, sigma){c1<-0.5; c2<-0.5;(mu*sigma+c1)*((1-mu)*sigma+c2)/((sigma+c1+c2)^2*(sigma+1+c1+c2))}), 
class = c("gamlss.family", "family"))
}
