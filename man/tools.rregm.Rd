\name{tools.rregm}
\alias{AIC.rregm}
\alias{BIC.rregm}
\alias{coef.rregm}
\alias{logLik.rregm}
\alias{print.rregm}
\alias{print.LRskov}
\alias{res}
\alias{summary.rregm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print a summary for a object of the "rregm" class.
}
\description{
Tools for a objects of the "rregm" class.
}
\usage{
res(object, type="pearson")
\method{AIC}{rregm}(object, ..., k=2)
\method{BIC}{rregm}(object, ...)
\method{coef}{rregm}(object, ...)
\method{logLik}{rregm}(object, ...)
\method{print}{LRskov}(x, digits = max(3L, getOption("digits") - 3L), ...)
\method{print}{rregm}(x, digits = max(3L, getOption("digits") - 3L), ...)
\method{summary}{rregm}(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, object}{
an object of the "rregm" class.
}
\item{type}{type of residuals to be presented: pearson (default), mod.pearson or quantile. 
}
\item{digits}{minimal number of significant digits
}
\item{k}{numeric, the penalty per parameter to be used; the default k = 2 is the classical AIC.
}
  \item{\dots}{
for extra arguments.}
}
\details{
Supported regression models are:
- reparametrized beta
- reparametrized gamma
- reparametrized beta prime
- reparametrized inverse gamma
- reparametrized log-normal
}
\value{
A complete summary for the coefficients extracted from a "rregm" or "LRskov" object.  
}
\references{
Bourguignon, M., Gallardo, D.I. (2025) A general and unified class of gamma regression models. 
Chemometrics and Intelligent Laboratory Systems, 261, 105382.
}
\author{
Diego Gallardo and Marcelo Bourguignon.}
\examples{
set.seed(2100)
n=100; x1=rnorm(max(n)) ##drawing covariates, the same for mu and sigma
mu=exp(0.5-0.4*x1); sigma=exp(-0.1+0.05*x1)
y=rRGA(n, mu, sigma, param="MD") ## model parameterized in the median
data=list(y=y, x1=x1)
aux.RGA=fit.RGA(y~x1, sigma.formula=~x1, data=data, param="MD")
summary(aux.RGA)
qqnorm(res(aux.RGA, type="mod.pearson"))
}
