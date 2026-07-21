fit.RIG <-
function(formula = formula(data), sigma.formula=~1, data, param="AM")
{
if (!inherits(formula, "formula")) {
        if (inherits(formula, "data.frame")) 
            warning("You gave a data.frame instead of a formula.")
        stop("formula is not an object of type formula")
    }
if (!inherits(sigma.formula, "formula")) {
        if (inherits(sigma.formula, "data.frame")) 
            warning("You gave a data.frame instead of a formula for sigma.")
        stop("sigma.formula is not an object of type formula")
    }
if (missing(formula)) 
        stop("Missing formula")
    if (missing(data)) 
        stop("Missing data")
    y <- model.frame(formula, data)[[1]]
    X <- model.matrix(formula, data)
    Z <- matrix(1, nrow = length(y))
	colnames(Z)<-"(Intercept)"
    if (sigma.formula != ~1) {
        Z <- model.matrix(sigma.formula, data)
    }
    quiet <- function(x) {
        sink(tempfile())
        on.exit(sink())
        invisible(force(x))
    }
  if (!any(param == c("AM", "GM", "HM", "MO", "MD"))) stop("param is not recognized")
family.aux=paste("IG",param,sep="")
tau = switch(param, AM = 1, GM = 0.5, MD = 1/3, MO = -1, HM = 0)
llike.RIG <- function(theta, y, x, z, tau) {
        r1 = ncol(x)
        r2 = ncol(z)
        beta <- theta[1:r1]
        nu <- theta[r1 + 1:r2]
        mu <- exp(x %*% beta)
        sigma <- ifelse(param=="MO",1,0)+exp(z %*% nu)
        alpha = sigma + tau
        beta = sigma*mu
        -sum(dinvgamma(y, shape = alpha, rate = beta, log = TRUE))
    }
    aux1 <- quiet(gamlss(1/y ~ 1, sigma.formula = ~1, 
        data = data, family = GA))
	if(ncol(X)>1)
	{
    aux1 <- quiet(gamlss(1/y ~ X[, -1], sigma.formula = ~1, 
        data = data, family = GA))
	if(ncol(Z)>1)
	{
    aux1 <- quiet(gamlss(1/y ~ X[, -1,drop=FALSE], sigma.formula = ~Z[, -1,drop=FALSE], 
        data = data, family = GA))
	}
	}
    inits <- c(aux1$mu.coefficients, aux1$sigma.coefficients)
    maxi <- optim(inits, llike.RIG, y = y, x = X, z = Z, tau = tau, 
        hessian = TRUE, control = list(maxit = 1e+05))
    desv <- sqrt(diag(solve(maxi$hessian)))
    aa <- cbind(maxi$par, desv)
    rownames(aa) <- c(paste("beta_", colnames(X), sep = ""), 
        paste("nu_", colnames(Z), sep = ""))
    colnames(aa) = c("Estimate", "Std. Error")
    mu <- exp(X %*% aa[1:ncol(X), 1])
    sigma <- ifelse(param=="MO",1,0)+exp(Z %*% aa[ncol(X) + 1:ncol(Z), 1])
alpha=sigma+tau
beta=mu*sigma
y <- model.frame(formula, data)[[1]]
pearson=NULL
if(all(alpha>2))
{
mean.y=beta/(alpha-1)
sd.y=sqrt(beta^2/((alpha-1)^2*(alpha-2)))
pearson=(y-mean.y)/sd.y
}
##mod. pearson based on log(1/Y)
mean.log=digamma(alpha)-log(beta)
sd.log=sqrt(trigamma(alpha))
mod.pearson=(log(1/y)-mean.log)/sd.log
pF1=get(paste("p",family.aux,sep=""))
quant=qnorm(pF1(y, mu=mu, sigma=sigma))
val <- list(estimate = aa, logLik = -maxi$value, AIC = 2 * 
        maxi$value + 2 * nrow(aa), BIC = 2 * maxi$value + log(length(y)) * 
        nrow(aa), tau=tau, pearson.res = as.vector(pearson), 
        mod.pearson.res = as.vector(mod.pearson), quant.res = c(quant), 
        convergence = ifelse(maxi$convergence == 0, TRUE, FALSE), 
        dist = "IG", param = param, mu.x = X, sigma.x = Z)
class(val) <- "rregm"
val
}
