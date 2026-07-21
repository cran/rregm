\name{RBP}
\alias{fit.RBP}
\alias{BPAM}
\alias{BPGM}
\alias{BPHM}
\alias{BPMD}
\alias{BPMO}
\alias{dBPAM}
\alias{dBPGM}
\alias{dBPHM}
\alias{dBPMD}
\alias{dBPMO}
\alias{pBPAM}
\alias{pBPGM}
\alias{pBPHM}
\alias{pBPMD}
\alias{pBPMO}
\alias{qBPAM}
\alias{qBPGM}
\alias{qBPHM}
\alias{qBPMD}
\alias{qBPMO}
\alias{rBPAM}
\alias{rBPGM}
\alias{rBPHM}
\alias{rBPMD}
\alias{rBPMO}
\alias{dRBP}
\alias{pRBP}
\alias{qRBP}
\alias{rRBP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Tools for a reparameterized beta prime regression model
}
\description{
A set of functions related to the reparameterized beta prime regression model based on different measures of 
central tendency: mean, median, mode, geometric mean or harmonic mean. 
}
\usage{
BPAM(mu.link = "log", sigma.link = "log") 
BPGM(mu.link = "log", sigma.link = "log") 
BPHM(mu.link = "log", sigma.link = "log") 
BPMD(mu.link = "log", sigma.link = "log") 
BPMO(mu.link = "log", sigma.link = "log") 
dBPAM(x, mu = 1, sigma = 1.5, log = FALSE) 
dBPGM(x, mu = 1, sigma = 1.5, log = FALSE) 
dBPHM(x, mu = 1, sigma = 1.5, log = FALSE) 
dBPMD(x, mu = 1, sigma = 1.5, log = FALSE) 
dBPMO(x, mu = 1, sigma = 1.5, log = FALSE) 
dRBP(x, mu=1, sigma=1.5, param="AM", log=FALSE)
fit.RBP(formula = formula(data), sigma.formula=~1, data, param="AM")
pBPAM(q, mu = 1, sigma = 1.5, lower.tail = TRUE, log.p = FALSE) 
pBPGM(q, mu = 1, sigma = 1.5, lower.tail = TRUE, log.p = FALSE) 
pBPHM(q, mu = 1, sigma = 1.5, lower.tail = TRUE, log.p = FALSE) 
pBPMD(q, mu = 1, sigma = 1.5, lower.tail = TRUE, log.p = FALSE) 
pBPMO(q, mu = 1, sigma = 1.5, lower.tail = TRUE, log.p = FALSE)
pRBP(q, mu = 1, sigma=1.5, param="AM", lower.tail = TRUE, log.p = FALSE) 
qBPAM(p, mu = 1, sigma = 1.5, lower.tail = TRUE, log.p = FALSE) 
qBPGM(p, mu = 1, sigma = 1.5, lower.tail = TRUE, log.p = FALSE) 
qBPHM(p, mu = 1, sigma = 1.5, lower.tail = TRUE, log.p = FALSE) 
qBPMD(p, mu = 1, sigma = 1.5, lower.tail = TRUE, log.p = FALSE) 
qBPMO(p, mu = 1, sigma = 1.5, lower.tail = TRUE, log.p = FALSE) 
qRBP(p, mu = 1, sigma=1.5, param="AM", lower.tail = TRUE, log.p = FALSE)
rBPAM(n, mu = 1, sigma = 1.5) 
rBPGM(n, mu = 1, sigma = 1.5) 
rBPHM(n, mu = 1, sigma = 1.5) 
rBPMD(n, mu = 1, sigma = 1.5) 
rBPMO(n, mu = 1, sigma = 1.5) 
rRBP(n, mu = 1, sigma=1.5, param="AM")
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
The parameterization for the reparameterized beta prime distribution is given by
\deqn{
f(x; \mu, \sigma) = \frac{x^{\mu\,\sigma + \tau_1 -1}(1 + x)^{-(\sigma(1+\mu) + \tau_1-\tau_2+1)}}{B(\mu\,\sigma + \tau_1, \sigma - \tau_2 + 1)}, \quad x>0,
}
where \eqn{\delta=\delta(\sigma)=(\sqrt{\sigma(\sigma+4)}+\sigma)/2}, \eqn{0 < \mu < 1}, \eqn{\sigma > 0} and \eqn{\tau} 
is a constant. The following cases are highlighted:

- param="AM": \eqn{\tau=0} and \eqn{\mu} represents the mean of the distribution.

- param="GM": \eqn{\tau=1/2} and \eqn{\mu} represents the geometric mean of the distribution.

- param="MD": \eqn{\tau=1/3} and \eqn{\mu} represents the median of the distribution.

- param="MO" or ="HM": \eqn{\tau=1} and \eqn{\mu} represents the mode or the harmonic mean of the distribution.

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
r_i=\frac{y_i-m_i}{s_i}, \quad i=1,\ldots,n,
} 
where 
\deqn{
m_i=\frac{\mu_i \sigma_i+\tau_1}{\sigma_i-\tau_2+1} \quad \mbox{and} \quad s_i=\frac{(\mu_i \sigma_i+\tau_1)((1+\mu_i)\sigma_i+\tau_1-\tau_2+1)}{(\sigma_i+\tau-1)}(\sigma_i-\tau_2)^{-1/2}.
}
Note that the Pearson's residuals are well defined as long as \eqn{\sigma_i-\tau_2>1}, \eqn{\forall i=1,\ldots,n}.
On the other hand, the modified Pearson's residuals are given by\deqn{
r_i^*=\frac{\mbox{logit}(y_i)-m_i^*}{s_i^*}, \quad i=1,\ldots,n,
} 
where 
\deqn{
m_i^*=\psi(\mu_i \sigma_i+\tau_1)-\psi((1+\mu_i)\sigma_i+\tau_1-\tau_2+1) \quad \mbox{and} \quad s_i^*=\sqrt{\psi'(\mu_i \sigma_i+\tau_1)-\psi'((1+\mu_i)\sigma_i+\tau_1-\tau_2+1)},
}
with \eqn{\psi(\cdot)} and \eqn{\psi'(\cdot)} denoting the digamma and trigamma functions, respectively.
Finally, the quantile residuals are given by
\deqn{
r_i^q=\Phi^{-1}\left(I_{\frac{y_i}{1+y_i}}(\mu_i \sigma_i+\tau_1,(1+\mu_i)\sigma_i+\tau_1-\tau_2+1)\right), \quad i=1,\ldots,n,
} 
where \eqn{\Phi^{-1}(\cdot)} denotes the inverse of the cumulative distribution function for the standard normal model and \eqn{I_y(\alpha,\beta)=B_x(\alpha, \beta)/B(\alpha, \beta)} 
is the incomplete beta function ratio, \eqn{B_x(\alpha, \beta) = \int_{0}^{x}\omega^{\alpha-1}(1-\omega)^{\beta-1}\textrm{d} \omega} is the incomplete beta function,
\eqn{B(\alpha, \beta) = \Gamma(\alpha)\Gamma(\beta)/\Gamma(\alpha + \beta)} is the beta function and
\eqn{\Gamma(\alpha) = \int_{0}^{\infty}\omega^{\alpha-1}\textrm{e}^{-\omega}\textrm{d} \omega} is the gamma function.
dRBP gives the density, pRBP gives the distribution function, qRBP gives the quantile function, and rRBP generates random deviates from the beta prime distribution with the specified
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
\item{dist}{BP (the beta prime distribution).}
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
fit.RIG,
fit.RLN,
fit.RLL
}
\examples{
set.seed(2100)
n=100; x1=rnorm(max(n)) ##drawing covariates, the same for mu and sigma
mu=exp(0.5-0.4*x1); sigma=exp(-0.1+0.05*x1)
y=rRBP(n, mu, sigma, param="MD") ## model parameterized in the median
data=list(y=y, x1=x1)
aux.RBP=fit.RBP(y~x1, sigma.formula=~x1, data=data, param="MD")
summary(aux.RBP)
qqnorm(res(aux.RBP, type="mod.pearson"))
#The beta prime model parameterized in the median also can be fitted using gamlss
#gamlss(y~x1, sigma.formula=~x1, data=data, family=BPMD)
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
