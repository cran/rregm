\name{RIG}
\alias{fit.RIG}
\alias{IGAM}
\alias{IGGM}
\alias{IGHM}
\alias{IGMD}
\alias{IGMO}
\alias{dIGAM}
\alias{dIGGM}
\alias{dIGHM}
\alias{dIGMD}
\alias{dIGMO}
\alias{pIGAM}
\alias{pIGGM}
\alias{pIGHM}
\alias{pIGMD}
\alias{pIGMO}
\alias{qIGAM}
\alias{qIGGM}
\alias{qIGHM}
\alias{qIGMD}
\alias{qIGMO}
\alias{rIGAM}
\alias{rIGGM}
\alias{rIGHM}
\alias{rIGMD}
\alias{rIGMO}
\alias{dRIG}
\alias{pRIG}
\alias{qRIG}
\alias{rRIG}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Tools for a reparameterized inverse gamma regression model
}
\description{
A set of functions related to the reparameterized inverse gamma regression model based on different measures of 
central tendency: mean, median, mode, geometric mean or harmonic mean. 
}
\usage{
IGAM(mu.link = "log", sigma.link = "log") 
IGGM(mu.link = "log", sigma.link = "log") 
IGHM(mu.link = "log", sigma.link = "log") 
IGMD(mu.link = "log", sigma.link = "log") 
IGMO(mu.link = "log", sigma.link = "logshiftto1") 
dIGAM(x, mu = 1, sigma = 1, log = FALSE) 
dIGGM(x, mu = 1, sigma = 1, log = FALSE) 
dIGHM(x, mu = 1, sigma = 1, log = FALSE) 
dIGMD(x, mu = 1, sigma = 1, log = FALSE) 
dIGMO(x, mu = 1, sigma = 1.5, log = FALSE) 
dRIG(x, mu=1, sigma=1.5, param="AM", log=FALSE)
fit.RIG(formula = formula(data), sigma.formula=~1, data, param="AM")
pIGAM(q, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE) 
pIGGM(q, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE) 
pIGHM(q, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE) 
pIGMD(q, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE) 
pIGMO(q, mu = 1, sigma = 1.5, lower.tail = TRUE, log.p = FALSE)
pRIG(q, mu=1, sigma=1.5, param="AM", lower.tail = TRUE, log.p = FALSE) 
qIGAM(p, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE) 
qIGGM(p, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE) 
qIGHM(p, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE) 
qIGMD(p, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE) 
qIGMO(p, mu = 1, sigma = 1.5, lower.tail = TRUE, log.p = FALSE) 
qRIG(p, mu=1, sigma=1.5, param="AM", lower.tail = TRUE, log.p = FALSE)
rIGAM(n, mu = 1, sigma = 1) 
rIGGM(n, mu = 1, sigma = 1) 
rIGHM(n, mu = 1, sigma = 1) 
rIGMD(n, mu = 1, sigma = 1) 
rIGMO(n, mu = 1, sigma = 1.5) 
rRIG(n, mu=1, sigma=1.5, param="AM")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{mu.link}{the mu link function with default log
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
The parameterization for the reparameterized inverse gamma distribution is given by
\deqn{
f(y; \mu, \sigma) = \frac{(\sigma \mu)^{\sigma + \tau}}{\Gamma(\sigma + \tau)}y^{-\sigma - \tau-1}\textrm{e}^{-\frac{\sigma \mu}{y}}, \quad y > 0,
}
where \eqn{0 < \mu < 1}, \eqn{\sigma > 0} and \eqn{\tau} is a constant. The following cases are highlighted:

- param="AM": \eqn{\tau=1} and \eqn{\mu} represents the mean of the distribution.

- param="GM": \eqn{\tau=1/2} and \eqn{\mu} represents the geometric mean of the distribution.

- param="HM": \eqn{\tau=0} and \eqn{\mu} represents the harmonic mean of the distribution.

- param="MO": \eqn{\tau=-1} and \eqn{\mu} represents the mode of the distribution.

- param="MD": \eqn{\tau=1/2} and \eqn{\mu} represents the median of the distribution.

Suppose the central tendency and the concentration parameter of \eqn{Y_i} satisfies the following functional relations
\deqn{
\log(\mu_i) = \mathbf{x}^\top_i\bm{\xi} \quad \textrm{and} \quad \log(\sigma_i) = \eta_{2i} = \mathbf{z}^\top_i\bm{\nu},
}
\eqn{\bm{\xi} = (\xi_1, \ldots, \xi_p)^\top} and \eqn{\bm{\nu} = (\nu_1, \ldots, \nu_q)^\top}
are vectors of unknown regression coefficients which are assumed to be functionally independent,
\eqn{\bm{\xi} \in \mathbb{R}^p} and \eqn{\bm{\nu} \in \mathbb{R}^q}, with \eqn{p + q < n},
and \eqn{\mathbf{x}_i = (x_{i1}, \ldots, x_{ip})^\top}
and \eqn{\mathbf{z}_i = (z_{i1}, \ldots, z_{iq})^\top} are observations on \eqn{p} and \eqn{q} known regressors, for \eqn{i = 1, \ldots, n}. 
Furthermore, we assume that the covariate matrices \eqn{\mathbf{X} = (\mathbf{x}_1, \ldots, \mathbf{x}_n)^\top} and 
\eqn{\mathbf{Z} = (\mathbf{z}_1, \ldots, \mathbf{z}_n)^\top} have rank \eqn{p} and \eqn{q}, respectively.

For this model, the Pearson's residuals are given by
\deqn{
r_i=\frac{y_i-m_i}{s_i}, \quad i=1,\ldots,n,
} 
where 
\deqn{
m_i=\frac{\mu_i \sigma_i}{\sigma_i+\tau_2} \quad \mbox{and} \quad s_i=\frac{\mu_i \sigma_i}{(\sigma_i+\tau-1)}(\sigma_i+\tau-2)^{-1/2}.
}
Note that the Pearson's residuals are well defined as long as \eqn{\sigma_i+\tau>2}, \eqn{\forall i=1,\ldots,n}.
On the other hand, the modified Pearson's residuals are given by
\deqn{
r_i^*=\frac{-\log(y_i)-m_i^*}{s_i^*}, \quad i=1,\ldots,n,
} 
where 
\deqn{
m_i^*=\psi(\sigma_i+\tau)-\log(\mu_i \sigma_i) \quad \mbox{and} \quad s_i^*=\sqrt{\psi'(\sigma_i+\tau)},
}
with \eqn{\psi(\cdot)} and \eqn{\psi'(\cdot)} denoting the digamma and trigamma functions, respectively.
Finally, the quantile residuals are given by
\deqn{
r_i^q=\Phi^{-1}\left(\frac{\gamma(\sigma_i+\tau,\mu_i \sigma_i/y_i)}{\Gamma(\sigma_i+\tau)}\right), \quad i=1,\ldots,n,
} 
where \eqn{\Phi^{-1}(\cdot)} denotes the inverse of the cumulative distribution function for the standard normal model and
\eqn{\gamma(a,z) = \int_{0}^{z}t^{a-1}\textrm{e}^{-t}\textrm{d}t} is the lower incomplete gamma
function and \eqn{\Gamma(\alpha) = \int_{0}^{\infty}\omega^{\alpha-1}\textrm{e}^{-\omega}\textrm{d} \omega} is the gamma function.
dRIG gives the density, pRIG gives the distribution function, qRIG gives the quantile function, and rRIG generates random deviates from the inverse gamma distribution with the specified
parameterization. In addition, dIGXX, pIGXX, qIGXX and rIGXX also provides the equivalent functions for a specified parameterization for XX: AM (mean), GM (geometric mean), 
HM (harmonic mean), MD (median) and MO (mode). For instance, dIGAM gives the density for the inverse gamma model parameterized in the mean, pIGGM gives the distribution function for the
inverse gamma model parameterized in the geometric mean and so on. 
Finally, the functions IGAM, IGGM, IGHM, IGMD and IGMO also provide a framework to fit models with \cite{gamlss}.
}

\value{
an object of class "rregm" is returned. The object returned for this functions is a list containing the following
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
\item{dist}{IG (the inverse gamma distribution).}
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
\examples{
set.seed(2100)
n=100; x1=rnorm(max(n)) ##drawing covariates, the same for mu and sigma
mu=exp(0.5-0.4*x1); sigma=exp(-0.1+0.05*x1)
y=rRIG(n, mu, sigma, param="MD") ## model parameterized in the median
data=list(y=y, x1=x1)
aux.RIG=fit.RIG(y~x1, sigma.formula=~x1, data=data, param="MD")
summary(aux.RIG)
qqnorm(res(aux.RIG, type="mod.pearson"))
#The inverse gamma model parameterized in the median also can be fitted using gamlss
#gamlss(y~x1, sigma.formula=~x1, data=data, family=IGMD)
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
