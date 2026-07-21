LL4 <- function(mu.link="log",
                sigma.link="logshiftto1")
{

mstats <-
checklink("mu.link",
          "LL4",
          substitute(mu.link),
          c("inverse","log","identity","own"))

    dstats <- checklink("sigma.link", "log-logistic4", substitute(sigma.link), 
                                 c("logshiftto1","inverse", "log", "identity", "own"))
cot <- function(x) 1/tan(x)

structure(

list(

family=c("LL4","Log-logistic harmonic"),

parameters=list(mu=TRUE,sigma=TRUE),

nopar=2,

type="Continuous",

mu.link=as.character(substitute(mu.link)),
sigma.link=as.character(substitute(sigma.link)),

mu.linkfun=mstats$linkfun,
sigma.linkfun=dstats$linkfun,

mu.linkinv=mstats$linkinv,
sigma.linkinv=dstats$linkinv,

mu.dr=mstats$mu.eta,
sigma.dr=dstats$mu.eta,

####################################
## SCORE
####################################

dldm=function(y,mu,sigma)
{
lambda <- mu*(pi/sigma)/sin(pi/sigma)
z <- (y/lambda)^sigma
sigma/mu*(2*z/(1+z)-1)
},

dldd = function(y, mu, sigma)
{
    lambda <- mu*(pi/sigma)/sin(pi/sigma)
    z <- (y/lambda)^sigma
    L <- log(y/lambda)
    C <- (pi/sigma)*cos(pi/sigma)/sin(pi/sigma)
    1/sigma + L + 1 - C -
        2*z/(1+z)*(L + 1 - C)
},
d2ldm2 = function(y,mu,sigma){-(sigma^2)/(3*mu^2)}, 
d2ldd2 = function(y, mu, sigma){-(1/3 + pi^2/9)/sigma^2-(1/3)*(1 - (pi/sigma)*cot(pi/sigma))^2},
d2ldmdd = function(y,mu, sigma){(sigma - pi*cot(pi/sigma))/(3*mu)},
G.dev.incr=function(y,mu,sigma,w,...)
{
-2*dLL4(y,mu,sigma,log=TRUE)
},

rqres=expression(
rqres(
pfun="pLL4",
type="Continuous",
y=y,
mu=mu,
sigma=sigma)
),

mu.initial=expression(
{
mu <- (y+mean(y))/2
}
),

sigma.initial=expression(
{
sigma <- rep(2,length(y))
}
),

mu.valid=function(mu) all(mu>0),

sigma.valid=function(sigma) all(sigma>1),

y.valid=function(y) all(y>0)

),

class=c("gamlss.family","family")

)

}
