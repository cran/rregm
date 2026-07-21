\name{RLN}
\alias{fit.RLN}
\alias{LNAM}
\alias{LNGM}
\alias{LNHM}
\alias{LNMD}
\alias{LNMO}
\alias{dLNAM}
\alias{dLNGM}
\alias{dLNHM}
\alias{dLNMD}
\alias{dLNMO}
\alias{pLNAM}
\alias{pLNGM}
\alias{pLNHM}
\alias{pLNMD}
\alias{pLNMO}
\alias{qLNAM}
\alias{qLNGM}
\alias{qLNHM}
\alias{qLNMD}
\alias{qLNMO}
\alias{rLNAM}
\alias{rLNGM}
\alias{rLNHM}
\alias{rLNMD}
\alias{rLNMO}
\alias{dRLN}
\alias{pRLN}
\alias{qRLN}
\alias{rRLN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Tools for a reparameterized log-normal regression model
}
\description{
A set of functions related to the reparameterized log-normal regression model based on different measures of 
central tendency: mean, median, mode, geometric mean or harmonic mean. 
}
\usage{
LNAM(mu.link = "log", sigma.link = "log") 
LNGM(mu.link = "log", sigma.link = "log") 
LNHM(mu.link = "log", sigma.link = "log") 
LNMD(mu.link = "log", sigma.link = "log") 
LNMO(mu.link = "log", sigma.link = "log") 
dLNAM(x, mu = 1, sigma = 1, log = FALSE) 
dLNGM(x, mu = 1, sigma = 1, log = FALSE) 
dLNHM(x, mu = 1, sigma = 1, log = FALSE) 
dLNMD(x, mu = 1, sigma = 1, log = FALSE) 
dLNMO(x, mu = 1, sigma = 1, log = FALSE) 
dRLN(x, mu=1, sigma=1, param="AM", log=FALSE)
fit.RLN(formula = formula(data), sigma.formula=~1, data, param="AM")
pLNAM(q, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE) 
pLNGM(q, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE) 
pLNHM(q, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE) 
pLNMD(q, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE) 
pLNMO(q, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE)
pRLN(q, mu=1, sigma=1, param="AM", lower.tail = TRUE, log.p = FALSE) 
qLNAM(p, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE) 
qLNGM(p, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE) 
qLNHM(p, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE) 
qLNMD(p, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE) 
qLNMO(p, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE) 
qRLN(p, mu=1, sigma=1, param="AM", lower.tail = TRUE, log.p = FALSE)
rLNAM(n, mu = 1, sigma = 1) 
rLNGM(n, mu = 1, sigma = 1) 
rLNHM(n, mu = 1, sigma = 1) 
rLNMD(n, mu = 1, sigma = 1) 
rLNMO(n, mu = 1, sigma = 1) 
rRLN(n, mu=1, sigma=1, param="AM")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{mu.link}{the mu link function with default logit
}
\item{sigma.link}{the sigma link function with default log
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
parameterization used for the model. "AM" for mean, "MD" for median, "MO" for mode, "GM" for geometric mean, and "HM" for 
harmonic mean.  
}
\item{x, q}{vector of quantiles
}
\item{p}{vector of probabilities
}
\item{n}{number of observations. If \eqn{\mbox{length}(n) > 1}, the length is taken to be the number required.
}
\item{log, log.p}{logical; if TRUE, probabilities p are given as log(p).
}
\item{lower.tail}{logical; if TRUE, probabilities are \eqn{P(X \leq x)} otherwise, \eqn{P(X>x)}.
}
}
\details{
The parameterization for the reparameterized log-normal distribution is given by
\deqn{
f(x; \mu, \sigma) = \dfrac{1}{x\,\phi\sqrt{2\pi}} 
\exp\!\left[-\dfrac{(\ln x - \theta)^2}{2\phi^2}\right], \quad y > 0,
}
where \eqn{\theta=\log(\mu)-\tau \phi}, \eqn{\phi=\sqrt{\sigma}}, \eqn{\mu > 0}, \eqn{\sigma > 0} and \eqn{\tau} 
is a constant. The following cases are highlighted:

- param="MO": \eqn{\tau=-1} and \eqn{\mu} represents the mode of the distribution.

- param="HM": \eqn{\tau=-1/2} and \eqn{\mu} represents the harmonic mean of the distribution.

- param="MD" or "GM": \eqn{\tau=0} and \eqn{\mu} represents the median or the geometric mean of the distribution.

- param="AM": \eqn{\tau=1/2} and \eqn{\mu} represents the mean of the distribution.


Suppose the central tendency and the concentration parameter of \eqn{Y_i} satisfies the following functional relations
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

For this model, the Pearson's residuals are given by
\deqn{
r_i=\frac{y_i-exp(\theta_i+\phi_i^2/2)}{\sqrt((\exp(\phi_i^2)-1)\exp(2*\theta_i+\phi_i^2))}, \quad i=1,\ldots,n.
} 
On the other hand, the modified Pearson's residuals are given by
\deqn{
r_i^*=\frac{\log(y_i)-\theta_i}{\phi_i}, \quad i=1,\ldots,n,
} 
which coincide with the quantile residuals.
dRLN gives the density, pRLN gives the distribution function, qRLN gives the quantile function, and rRLN generates random deviates from the log-normal distribution with the specified
parameterization. 
}

\value{
an object of class "rregm" is returned. The object returned for these functions is a list containing the following
components:
\item{estimate}{A matrix containing the estimates and standard errors.}
\item{logLik}{the log-likelihood function evaluated at the corresponding estimators.}
\item{AIC}{the Akaike information criterion.}
\item{BIC}{the Bayesian information criterion.}
\item{tau1, tau2}{values for tau1 and tau2, depending on the considered parameterization.}
\item{pearson.res}{Pearson's residuals.}
\item{mod.pearson.res}{modified Pearson's residuals.}
\item{quant.res}{quantile residuals.}
\item{convergence}{logical. If convergence was attained.}
\item{dist}{LN (the log-normal distribution).}
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
y=rRLN(n, mu, sigma, param="MD") ## model parameterized in the median
data=list(y=y, x1=x1)
aux.RLN=fit.RLN(y~x1, sigma.formula=~x1, data=data, param="MD")
summary(aux.RLN)
qqnorm(res(aux.RLN, type="mod.pearson"))
#The log-normal model parameterized in the median also can be fitted using gamlss
#gamlss(y~x1, sigma.formula=~x1, data=data, family=LNMD)
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
