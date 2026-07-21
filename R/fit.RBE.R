fit.RBE<-function (formula = formula(data), sigma.formula = ~1, data, 
    param = "AM") 
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
    family.aux=paste("BE",param,sep="")
    if (!any(param == c("AM", "GM", "HM", "MO", "MD"))) 
        stop("param is not recognized")
  tau1=switch(param, AM=0, GM=0.5, MD=1/3, MO=1, HM=1)
  tau2=switch(param, AM=0, GM=0.5, MD=2/3, MO=2, HM=1)
    quiet <- function(x) {
        sink(tempfile())
        on.exit(sink())
        invisible(force(x))
    }
    llike.RBE <- function(theta, y, x, z, tau1, tau2) {
        r1 = ncol(x)
        r2 = ncol(z)
        beta <- theta[1:r1]
        nu <- theta[r1 + 1:r2]
        mu <- plogis(x %*% beta)
        sigma <- exp(z %*% nu)
        alpha = mu*sigma + tau1
        beta = (1-mu)*sigma + tau2 - tau1
        -sum(dbeta(y, shape1 = alpha, shape2 = beta, log = TRUE))
    }
    aux1 <- quiet(gamlss(qlogis(y) ~ 1, sigma.formula = ~1, 
        data = data, family = NO))
	if(ncol(X)>1)
	{
    aux1 <- quiet(gamlss(qlogis(y) ~ X[, -1], sigma.formula = ~1, 
        data = data, family = NO))
	if(ncol(Z)>1)
	{
    aux1 <- quiet(gamlss(qlogis(y) ~ X[, -1,drop=FALSE], sigma.formula = ~Z[, -1,drop=FALSE], 
        data = data, family = NO))
	}
	}
    inits <- c(aux1$mu.coefficients, aux1$sigma.coefficients)
    maxi <- optim(inits, llike.RBE, y = y, x = X, z = Z, tau1 = tau1, tau2=tau2, 
        hessian = TRUE, control = list(maxit = 1e+05))
    desv <- sqrt(diag(solve(maxi$hessian)))
    aa <- cbind(maxi$par, desv)
    rownames(aa) <- c(paste("beta_", colnames(X), sep = ""), 
        paste("nu_", colnames(Z), sep = ""))
    colnames(aa) = c("Estimate", "Std. Error")
    mu <- plogis(X %*% aa[1:ncol(X), 1])
    sigma <- exp(Z %*% aa[ncol(X) + 1:ncol(Z), 1])
    alpha = mu*sigma + tau1
    beta = (1-mu)*sigma + tau2 - tau1
    y <- model.frame(formula, data)[[1]]
mean.y=alpha/(alpha+beta)
sd.y=sqrt(alpha*beta/((alpha+beta)^2*(alpha+beta+1)))
pearson=(y-mean.y)/sd.y
##mod. pearson based on log(Y/(1-Y))
mean.log=digamma(alpha)-digamma(beta)
sd.log=sqrt(trigamma(alpha)+trigamma(beta))
mod.pearson=(log(y/(1-y))-mean.log)/sd.log
pF1=get(paste("p",family.aux,sep=""))
quant=qnorm(pF1(y, mu=mu, sigma=sigma))
val <- list(estimate = aa, logLik = -maxi$value, AIC = 2 * 
        maxi$value + 2 * nrow(aa), BIC = 2 * maxi$value + log(length(y)) * 
        nrow(aa), tau1 = tau1, tau2=tau2, pearson.res = as.vector(pearson), 
        mod.pearson.res = as.vector(mod.pearson), quant.res = c(quant), 
        convergence = ifelse(maxi$convergence == 0, TRUE, FALSE), 
        dist = "GA", param = param, mu.x = X, sigma.x = Z)
class(val) <- "rregm"
val
}
