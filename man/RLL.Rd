\name{RLL}
\alias{fit.RLL}
\alias{fit.RLL.cens}
\alias{LL}
\alias{LL1}
\alias{LL2}
\alias{LL3}
\alias{LL4}
\alias{LL5}
\alias{LL6}
\alias{dLL}
\alias{dLL2}
\alias{dLL3}
\alias{dLL4}
\alias{dLL5}
\alias{dLL6}
\alias{pLL}
\alias{pLL2}
\alias{pLL3}
\alias{pLL4}
\alias{pLL5}
\alias{pLL6}
\alias{qLL}
\alias{qLL2}
\alias{qLL3}
\alias{qLL4}
\alias{qLL5}
\alias{qLL6}
\alias{rLL}
\alias{rLL2}
\alias{rLL3}
\alias{rLL4}
\alias{rLL5}
\alias{rLL6}
\alias{dRLL}
\alias{pRLL}
\alias{qRLL}
\alias{rRLL}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Tools for a reparameterized log-logistic regression model
}
\description{
A set of functions related to the reparameterized log-logistic regression model based on different 
parameterizations1: standard (LL), proportional odds (LL2), mean (LL3), harmonic mean (LL4), mode (LL5) and quantile (LL6). 
}
\usage{
LL(mu.link = "log", sigma.link = "log") 
LL2(mu.link = "log", sigma.link = "log") 
LL3(mu.link = "log", sigma.link = "logshiftto1") 
LL4(mu.link = "log", sigma.link = "logshiftto1") 
LL5(mu.link = "log", sigma.link = "logshiftto1") 
dLL(x, mu = 1, sigma = 1, log = FALSE)
dLL2(x, mu = 1, sigma = 1, log = FALSE)
dLL3(x, mu = 1, sigma = 1.5, log = FALSE)
dLL4(x, mu = 1, sigma = 1.5, log = FALSE)
dLL5(x, mu = 1, sigma = 1.5, log = FALSE)
dLL6(x, mu = 1, sigma = 1.5, nu=0.5, log = FALSE)
dRLL(x, mu=1, sigma=1.5, nu=0.5, param="LL", log=FALSE)
fit.RLL(formula = formula(data), sigma.formula = ~1, data, param = "LL", nu=0.5)
fit.RLL.cens(formula = formula(data), sigma.formula = ~1, data, param = "LL", nu = 0.5) 
pLL(q, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE) 
pLL2(q, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE) 
pLL3(q, mu = 1, sigma = 1.5, lower.tail = TRUE, log.p = FALSE) 
pLL4(q, mu = 1, sigma = 1.5, lower.tail = TRUE, log.p = FALSE) 
pLL5(q, mu = 1, sigma = 1.5, lower.tail = TRUE, log.p = FALSE) 
pLL6(q, mu = 1, sigma = 1.5, nu=0.5, lower.tail = TRUE, log.p = FALSE) 
pRLL(q, mu=1, sigma=1.5, nu=0.5, param="LL", lower.tail = TRUE, log.p = FALSE) 
qLL(p, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE) 
qLL2(p, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE) 
qLL3(p, mu = 1, sigma = 1.5, lower.tail = TRUE, log.p = FALSE) 
qLL4(p, mu = 1, sigma = 1.5, lower.tail = TRUE, log.p = FALSE) 
qLL5(p, mu = 1, sigma = 1.5, lower.tail = TRUE, log.p = FALSE) 
qLL6(p, mu = 1, sigma = 1.5, nu=0.5, lower.tail = TRUE, log.p = FALSE) 
qRLL(p, mu=1, sigma=1.5, nu=0.5, param="LL", log.p = FALSE)
rLL(n, mu = 1, sigma = 1) 
rLL2(n, mu = 1, sigma = 1) 
rLL3(n, mu = 1, sigma = 1.5) 
rLL4(n, mu = 1, sigma = 1.5) 
rLL5(n, mu = 1, sigma = 1.5) 
rLL6(n, mu = 1, sigma = 1.5, nu=0.5) 
rRLL(n, mu=1, sigma=1.5, nu=0.5, param="LL")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{mu.link}{the mu link function with default log.
}
\item{sigma.link}{the sigma link function with default log for parameterizations1 LL, LL2 and LL6 and logshiftto1 for parameterizations1 LL3, LL4 and LL5.
}
\item{mu, sigma}{vector of parameter values
}
  \item{formula}{
an object of class "\cite{formula}" (or one that can be coerced to that class): a symbolic description of the model to be fitted. The 
details of model specification are given under ‘Details’.
}
  \item{data}{
an optional data frame, list or environment (or object coercible by \cite{as.data.frame} to a data frame) containing the variables in 
the model. If not found in data, the variables are taken from environment(formula), typically the environment from which 
lm is called.
}
  \item{sigma.formula}{
a formula object for fitting a model to the sigma parameter, as in the formula above, e.g. sigma.formula=~x1+x2.
}
  \item{param}{
parameterization used for the model. "LL" for standard, "LL2" for proportional odds, "LL3" for mean, "LL4" for harmonic mean, "LL5" for mode and "LL6" for quantile.  
}
\item{x, q}{vector of quantiles
}
\item{p}{vector of probabilities
}
\item{n}{number of observations. If \eqn{\mbox{length}(n) > 1}, the length is taken to be the number required.
}
\item{nu}{quantile modeled (only for LL6 parametrization).
}
\item{log, log.p}{logical; if TRUE, probabilities p are given as log(p).
}
\item{lower.tail}{logical; if TRUE, probabilities are \eqn{P(X \leq x)} otherwise, \eqn{P(X>x)}.
}
}
\details{
The parameterization for the log-logistic distribution is given by
\deqn{
f(y; \rho,\lambda)=\frac{\rho}{\lambda}\frac{(t/\lambda)^{\rho-1}}{[1+(t/\lambda)^\rho]^2}, \quad y, \lambda, \rho >0.
}
The LL2--LL6 parameterizations1 corresponds to the following cases (\eqn{\psi=\pi/\rho}):

- "LL2": \eqn{\sigma=\rho} and \eqn{\mu=\lambda^{-\alpha}} (proportional odds). 

- "LL3": \eqn{\sigma=\rho} and \eqn{\mu=\psi\lambda/\sin(\psi)} represents the mean of the distribution.

- "LL4": \eqn{\sigma=\rho} and \eqn{\mu=\lambda/[\psi\sin(\psi)]} represents the harmonic mean of the distribution.

- "LL5": \eqn{\sigma=\rho} and \eqn{\mu=\lambda\left(\frac{\rho-1}{\rho+1}\right)^{1/\rho}} represents the mode of the distribution.

- "LL6": \eqn{\sigma=\rho} and \eqn{\mu=\lambda\left(\frac{\nu}{1-\nu}\right)^{1/\rho}} represents the \eqn{\nu}-th quantile of the distribution.

For the LL, LL2 and LL6 parameterizations1, suppose the parameters \eqn{\mu} and \eqn{\sigma} satisfies the following functional relations
\deqn{
\log(\mu_i) = \mathbf{x}^\top_i\bm{\xi} \quad \textrm{and} \quad \log(\sigma_i) = \eta_{2i} = \mathbf{z}^\top_i\bm{\nu},
}
where \eqn{\bm{\xi} = (\xi_1, \ldots, \xi_p)^\top} and \eqn{\bm{\nu} = (\nu_1, \ldots, \nu_q)^\top}
are vectors of unknown regression coefficients which are assumed to be functionally independent,
\eqn{\bm{\xi} \in \mathbb{R}^p} and \eqn{\bm{\nu} \in \mathbb{R}^q}, with \eqn{p + q < n},
and \eqn{\mathbf{x}_i = (x_{i1}, \ldots, x_{ip})^\top}
and \eqn{\mathbf{z}_i = (z_{i1}, \ldots, z_{iq})^\top} are observations on \eqn{p} and \eqn{q} known regressors, for \eqn{i = 1, \ldots, n}. 
Furthermore, we assume that the covariate matrices \eqn{\mathbf{X} = (\mathbf{x}_1, \ldots, \mathbf{x}_n)^\top} and 
\eqn{\mathbf{Z} = (\mathbf{z}_1, \ldots, \mathbf{z}_n)^\top} have rank \eqn{p} and \eqn{q}, respectively.

For the LL3, LL4 and LL5 parameterizations1, suppose \eqn{\sigma} satisfies the following functional relations
\deqn{
\log(\sigma_i-1) = \eta_{2i} = \mathbf{z}^\top_i\bm{\nu},
}

For this model, the Pearson's residuals are given by
\deqn{
r_i=\frac{y_i-m_{y_i}}{\sqrt(v_i)}, \quad i=1,\ldots,n,
} 
where \eqn{m_{y_i}=lambda_i*(pi/sigma_i)/sin(pi/sigma_i)} and \eqn{v_i=(2*lambda_i^2*(pi/sigma_i)/sin(2*pi/sigma_i)-m_{y_i}^2)}.
On the other hand, the modified Pearson's residuals are given by
\deqn{
r_i^*=\frac{y_i-\lambda_i}{\sqrt(V_i)}, \quad i=1,\ldots,n,
} 
where \eqn{V_i=pi/[sqrt(3)*sigma_i]}. 
dRLL gives the density, pRLL gives the distribution function, qRLL gives the quantile function, and rRLL generates random deviates from the log-logistic distribution with the specified
parameterization. 
}

\value{
an object of class "rregm" is returned. The object returned for these functions is a list containing the following
components:
\item{estimate}{A matrix containing the estimates and standard errors.}
\item{logLik}{the log-likelihood function evaluated at the corresponding estimators.}
\item{AIC}{the Akaike information criterion.}
\item{BIC}{the Bayesian information criterion.}
\item{pearson.res}{Pearson's residuals.}
\item{mod.pearson.res}{modified Pearson's residuals.}
\item{quant.res}{quantile residuals.}
\item{convergence}{logical. If convergence was attained.}
\item{dist}{LL (the log-logistic distribution).}
\item{param}{The specified parameterization.}
\item{mu.x}{design matrix for mu.}
\item{sigma.x}{design matrix for sigma.}
}
\references{
Bourguignon, M., Gallardo, D.I. (2025) A general and unified class of gamma regression models. 
Chemometrics and Intelligent Laboratory Systems, 261, 105382.
}
\author{
Diego Gallardo and Marcelo Bourguignon.
}
\seealso{
fit.RBE,
fit.RGA,
fit.RBP,
fit.RIG,
fit.RLL}
\examples{
set.seed(2100)
n=100; x1=rnorm(max(n)) ##drawing covariates, the same for mu and sigma
mu=exp(0.5-0.4*x1); sigma=exp(-0.1+0.05*x1)
y=rRLL(n, mu, sigma, param="LL2") ## model parameterized in the median
data=list(y=y, x1=x1)
aux.RLL=fit.RLL(y~x1, sigma.formula=~x1, data=data, param="LL2")
summary(aux.RLL)
qqnorm(res(aux.RLL, type="mod.pearson"))
#The log-logistic model with this parameterization also can be fitted using gamlss
#gamlss(y~x1, sigma.formula=~x1, data=data, family=LL2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
