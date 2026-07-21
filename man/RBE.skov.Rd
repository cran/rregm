\name{RBE.skov}
\alias{RBE.predictive}
\alias{RBE.skovgaard}
\title{
Skovgaard's adjustment and predictive measures for a reparameterized beta regression model
}
\description{
A set of functions related to Skovgaard's adjustment for the reparameterized beta regression model based 
on different measures of central tendency: mean, median, mode, geometric mean or harmonic mean. 
}
\usage{
RBE.skovgaard(formula, data, reduced.fo=~1, phi.test=NULL, param="AM")
RBE.predictive(formula, data, reduced.fo=~1, phi.test=NULL, train=0.7, param="AM")
}
\arguments{
  \item{formula}{
an object of class "\cite{formula}" (or one that can be coerced to that class): a symbolic description of the model to be fitted. The 
details of model specification are given under ‘Details’.
}
  \item{data}{
an optional data frame, list or environment (or object coercible by \cite{as.data.frame} to a data frame) containing the variables in 
the model. If not found in data, the variables are taken from environment(formula), typically the environment from which 
lm is called.
}
  \item{reduced.fo}{
a formula object for fitting the reduced model for \eqn{\mu}, reduced.fo=~1 by default.
}
  \item{param}{
parameterization used for the model. "AM" for mean, "MD" for median, "MO" for mode, "GM" for geometric mean, and "HM" for 
harmonic mean.  
}
\item{phi.test}{value to include for \eqn{\phi} in the null hypothesis. phi.test=NULL represents no hypothesis for \eqn{\phi}.
}
\item{train}{train proportion of observations used for model training. (0.7 by default)
}
}
\details{

The function \code{RBE.skovgaard()} performs likelihood ratio inference in
reparameterized beta (RB) regression models by computing the standard
likelihood ratio statistic together with two adjusted versions based on
Skovgaard's higher-order asymptotic approximation. These adjusted statistics
are designed to improve the finite-sample performance of likelihood ratio
tests by reducing the size distortions commonly observed when the sample size
is small or moderate.

The user specifies a full model through \code{formula} and a reduced model
through \code{reduced.fo}. The function then fits both models and returns the
standard likelihood ratio statistic (\code{SLR}) as well as the two adjusted
statistics (\code{SLR1} and \code{SLR2}), together with their corresponding
chi-squared p-values.

The function \code{RBE.predictive()} implements likelihood-based shrinkage
estimators for RB regression models. The data are randomly divided into
training and test sets according to the proportion specified in
\code{train}. Model fitting is performed using the training sample and
predictive performance is evaluated on the test sample.

Five competing estimators are considered:

\itemize{
\item \code{MLE}: maximum likelihood estimator;
\item \code{US}: classical uniform shrinkage estimator;
\item \code{US1}: shrinkage estimator based on the first Skovgaard-adjusted
likelihood ratio statistic;
\item \code{US2}: shrinkage estimator based on the second Skovgaard-adjusted
likelihood ratio statistic;
\item \code{GAIC}: variable selection based on the generalized Akaike
information criterion.
}

Predictive performance is summarized through the mean squared error (MSE),
mean absolute error (MAE), and logarithmic score (LS). Lower values of MSE
and MAE indicate better predictive accuracy, whereas larger values of LS
indicate better probabilistic predictions.

Both functions support all currently available parameterizations of the RB
distribution implemented in \pkg{rregm}, namely the arithmetic mean
(\code{"AM"}), geometric mean (\code{"GM"}), harmonic mean (\code{"HM"}),
mode (\code{"MO"}), and median (\code{"MD"}) parameterizations.

The methods implemented in these functions are based on higher-order
likelihood theory and likelihood-based shrinkage methodology, providing tools
for both improved finite-sample inference and enhanced predictive
performance in RB regression models.
}
\value{

Both functions return an object of class \code{"LRskov"}.

For \code{RBE.skovgaard()}, the returned object contains likelihood ratio statistics and associated p-values.

For \code{RBE.predictive()}, the returned object contains out-of-sample prediction measures (MSE, MAE, and LS) for the competing estimators, together with information on the training/test split and fitted model specification.
}
\references{
Bourguignon, M., Gallardo, D.I. (2025) A general and unified parameterization of the beta distribution: A flexible 
and robust beta regression model. Statistica Neerlandica, 79(2), e70007.
}
\author{
Diego Gallardo, Tiago Magalhaes and Rafael Izbicki.
}
\examples{

set.seed(123)
n <- 100
x1 <- runif(n)
x2 <- runif(n)
x3 <- runif(n)
mu <- plogis(-1 + 2*x1 - x2)
y <- rBEGM(n, mu = mu, sigma = 15)
dat <- data.frame(y, x1, x2, x3)
## Adjusted LR test
RBE.skovgaard(y ~ x1 + x2 + x3, data = dat, reduced.fo = ~ x1 + x2, param = "GM")

## Predictive shrinkage
set.seed(123)
RBE.predictive(y ~ x1 + x2 + x3, data = dat, train = 0.8, param = "GM")
}


