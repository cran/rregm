\name{RBE}
\alias{fit.RBE}
\alias{BEAM}
\alias{BEGM}
\alias{BEHM}
\alias{BEMD}
\alias{BEMO}
\alias{dBEAM}
\alias{dBEGM}
\alias{dBEHM}
\alias{dBEMD}
\alias{dBEMO}
\alias{pBEAM}
\alias{pBEGM}
\alias{pBEHM}
\alias{pBEMD}
\alias{pBEMO}
\alias{qBEAM}
\alias{qBEGM}
\alias{qBEHM}
\alias{qBEMD}
\alias{qBEMO}
\alias{rBEAM}
\alias{rBEGM}
\alias{rBEHM}
\alias{rBEMD}
\alias{rBEMO}
\alias{dRBE}
\alias{pRBE}
\alias{qRBE}
\alias{rRBE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Tools for a reparameterized beta regression model
}
\description{
A set of functions related to the reparameterized beta regression model based on different measures of 
central tendency: mean, median, mode, geometric mean or harmonic mean. 
}
\usage{
BEAM(mu.link = "logit", sigma.link = "log") 
BEGM(mu.link = "logit", sigma.link = "log") 
BEHM(mu.link = "logit", sigma.link = "log") 
BEMD(mu.link = "logit", sigma.link = "log") 
BEMO(mu.link = "logit", sigma.link = "log") 
dBEAM(x, mu = 0.5, sigma = 1, log = FALSE) 
dBEGM(x, mu = 0.5, sigma = 1, log = FALSE) 
dBEHM(x, mu = 0.5, sigma = 1, log = FALSE) 
dBEMD(x, mu = 0.5, sigma = 1, log = FALSE) 
dBEMO(x, mu = 0.5, sigma = 1, log = FALSE) 
dRBE(x, mu=0.5, sigma=1, param="AM", log=FALSE)
fit.RBE(formula = formula(data), sigma.formula=~1, data, param="AM")
pBEAM(q, mu = 0.5, sigma = 1, lower.tail = TRUE, log.p = FALSE) 
pBEGM(q, mu = 0.5, sigma = 1, lower.tail = TRUE, log.p = FALSE) 
pBEHM(q, mu = 0.5, sigma = 1, lower.tail = TRUE, log.p = FALSE) 
pBEMD(q, mu = 0.5, sigma = 1, lower.tail = TRUE, log.p = FALSE) 
pBEMO(q, mu = 0.5, sigma = 1, lower.tail = TRUE, log.p = FALSE)
pRBE(q, mu=0.5, sigma=1, param="AM", lower.tail = TRUE, log.p = FALSE) 
qBEAM(p, mu = 0.5, sigma = 1, lower.tail = TRUE, log.p = FALSE) 
qBEGM(p, mu = 0.5, sigma = 1, lower.tail = TRUE, log.p = FALSE) 
qBEHM(p, mu = 0.5, sigma = 1, lower.tail = TRUE, log.p = FALSE) 
qBEMD(p, mu = 0.5, sigma = 1, lower.tail = TRUE, log.p = FALSE) 
qBEMO(p, mu = 0.5, sigma = 1, lower.tail = TRUE, log.p = FALSE) 
qRBE(p, mu=0.5, sigma=1, param="AM", lower.tail = TRUE, log.p = FALSE)
rBEAM(n, mu = 0.5, sigma = 1) 
rBEGM(n, mu = 0.5, sigma = 1) 
rBEHM(n, mu = 0.5, sigma = 1) 
rBEMD(n, mu = 0.5, sigma = 1) 
rBEMO(n, mu = 0.5, sigma = 1) 
rRBE(n, mu=0.5, sigma=1, param="AM")
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
The parameterization for the reparameterized beta distribution is given by
\deqn{
f(x; \mu, \sigma) = \frac{x^{\mu\,\sigma + \tau_1-1}(1 - x)^{(1-\mu)\sigma + \tau_2-\tau_1-1}}{B(\mu\,\sigma + \tau_1, (1-\mu)\sigma + \tau_2-\tau_1)}, \quad 0 < x < 1,
}
where \eqn{0 < \mu < 1}, \eqn{\sigma > 0} and \eqn{\tau_1} and \eqn{\tau_2} are constant. The following cases are highlighted:

- param="AM": \eqn{\tau_1=\tau_2=0} and \eqn{\mu} represents the mean of the distribution.

- param="GM": \eqn{\tau_1=\tau_2=1/2} and \eqn{\mu} represents the geometric mean of the distribution.

- param="HM": \eqn{\tau_1=\tau_2=1} and \eqn{\mu} represents the harmonic mean of the distribution.

- param="MO": \eqn{\tau_1=1} and \eqn{\tau_2=2} and \eqn{\mu} represents the mode of the distribution.

- param="MD": \eqn{\tau_1=1/2} and \eqn{\tau_2=0} and \eqn{\mu} represents the median of the distribution.

Suppose the central tendency and the concentration parameter of \eqn{Y_i} satisfies the following functional relations
\deqn{
\mbox{logit}(\mu_i) = \mathbf{x}^\top_i\bm{\xi} \quad \textrm{and} \quad \log(\sigma_i) = \eta_{2i} = \mathbf{z}^\top_i\bm{\nu},
}
where \eqn{\mbox{logit}(u)=\log(u/(1-u))} is the logit function, \eqn{\bm{\xi} = (\xi_1, \ldots, \xi_p)^\top} and \eqn{\bm{\nu} = (\nu_1, \ldots, \nu_q)^\top}
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
m_i=\frac{\mu_i \sigma_i+\tau_1}{\sigma_i+\tau_2} \quad \mbox{and} \quad s_i=\sqrt{\frac{(\mu_i \sigma_i+\tau_1)((1-\mu_i)\sigma_i+\tau_2-\tau_1)}{(\sigma_i+\tau_2)^2(\sigma_i+\tau_2+1)}}.
}
whereas the modified Pearson's residuals are given by
\deqn{
r_i^*=\frac{\mbox{logit}(y_i)-m_i^*}{s_i^*}, \quad i=1,\ldots,n,
} 
where 
\deqn{
m_i^*=\psi(\mu_i \sigma_i+\tau_1)-\psi((1-\mu_i)\sigma_i+\tau_2-\tau_1) \quad \mbox{and} \quad s_i^*=\sqrt{\psi'(\mu_i \sigma_i+\tau_1)+\psi'((1-\mu_i)\sigma_i+\tau_2-\tau_1)},
}
with \eqn{\psi(\cdot)} and \eqn{\psi'(\cdot)} denoting the digamma and trigamma functions, respectively.
Finally, the quantile residuals are given by
\deqn{
r_i^q=\Phi^{-1}\left(I_{y_i}(\mu_i \sigma_i+\tau_1,(1-\mu_i)\sigma_i+\tau_2-\tau_1)\right), \quad i=1,\ldots,n,
} 
where \eqn{\Phi^{-1}(\cdot)} denotes the inverse of the cumulative distribution function for the standard normal model and \eqn{I_y(\alpha,\beta)=B_x(\alpha, \beta)/B(\alpha, \beta)} 
is the incomplete beta function ratio, \eqn{B_x(\alpha, \beta) = \int_{0}^{x}\omega^{\alpha-1}(1-\omega)^{\beta-1}\textrm{d} \omega} is the incomplete beta function,
\eqn{B(\alpha, \beta) = \Gamma(\alpha)\Gamma(\beta)/\Gamma(\alpha + \beta)} is the beta function and
\eqn{\Gamma(\alpha) = \int_{0}^{\infty}\omega^{\alpha-1}\textrm{e}^{-\omega}\textrm{d} \omega} is the gamma function.
dRBE gives the density, pRBE gives the distribution function, qRBE gives the quantile function, and rRBE generates random deviates from the beta distribution with the specified
parameterization. In addition, dBEXX, pBEXX, qBEXX and rBEXX also provide the equivalent functions for a specified parameterization for XX: AM (mean), GM (geometric mean), 
HM (harmonic mean), MD (median) and MO (mode). For instance, dBEAM gives the density for the beta model parameterized in the mean, pBEGM gives the distribution function for the
beta model parameterized in the geometric mean and so on. 
Finally, the functions BEAM, BEGM, BEHM, BEMD and BEMO also provide a framework to fit models with \cite{gamlss}.
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
\item{dist}{BE (the beta distribution).}
\item{param}{The specified parameterization.}
\item{mu.x}{design matrix for mu.}
\item{sigma.x}{design matrix for sigma.}
}
\references{
Bourguignon, M., Gallardo, D.I. (2025) A general and unified parameterization of the beta distribution: A flexible 
and robust beta regression model. Statistica Neerlandica, 79(2), e70007.
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
mu=plogis(0.5-0.4*x1); sigma=exp(-0.1+0.05*x1)
y=rRBE(n, mu, sigma, param="MD") ## model parameterized in the median
data=list(y=y, x1=x1)
aux.RBE=fit.RBE(y~x1, sigma.formula=~x1, data=data, param="MD")
summary(aux.RBE)
qqnorm(res(aux.RBE, type="mod.pearson"))
#The beta model parameterized in the median also can be fitted using gamlss
#gamlss(y~x1, sigma.formula=~x1, data=data, family=BEMD)
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
