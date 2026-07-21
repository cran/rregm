fit.RLL<-function (formula = formula(data), sigma.formula = ~1, data, 
    param = "LL", nu=0.5) 
{
    if (!any(param == c("LL", "LL2", "LL3", "LL4", "LL5","LL6"))) 
        stop("param is not recognized")
    quiet <- function(x) {
        sink(tempfile())
        on.exit(sink())
        invisible(force(x))
    }
    if(any(param==c("LL", "LL2", "LL3", "LL4","LL5")))
	{
    X <- model.matrix(formula, data)
    Z <- model.matrix(sigma.formula, data)
    y <- model.frame(formula, data)[[1]]
    llike.LL <- function(theta, y, x, z, param) {
        r1 = ncol(x)
        r2 = ncol(z)
        beta <- theta[1:r1]
        xi <- theta[r1 + 1:r2]
        mu <- exp(x %*% beta)
        sigma <- exp(z %*% xi)
	df1 = get(paste("d", param, sep = ""))
        -sum(df1(y, mu=mu, sigma=sigma, log = TRUE))
    }
    aux=lm(log(y)~X[,-1,drop=FALSE])
    inits <- c(coef(aux), rep(0, ncol(Z)))
    maxi <- optim(inits, llike.LL, y = y, x = X, z = Z, param = param, 
        hessian = TRUE, control = list(maxit = 1e+05))
    desv <- sqrt(diag(solve(maxi$hessian)))
    aa <- cbind(maxi$par, desv)
    rownames(aa) <- c(paste("beta_", colnames(X), sep = ""), 
        paste("nu_", colnames(Z), sep = ""))
    colnames(aa) = c("Estimate", "Std. Error")
    mu <- exp(X %*% aa[1:ncol(X), 1])
    sigma <- exp(Z %*% aa[ncol(X) + 1:ncol(Z), 1])
    lambda=switch(param, LL=mu, LL2=mu^(-1/sigma), LL3=mu*sin(pi/sigma)/(pi/sigma),
	LL4=mu*(pi/sigma)/sin(pi/sigma),LL5=mu*(sigma+1)/(sigma-1))
    pearson = NULL
    if (all(sigma > 2)) {
        mean.y = lambda*(pi/sigma)/sin(pi/sigma)
        sd.y = sqrt((2*lambda^2*(pi/sigma)/sin(2*pi/sigma)-mean.y^2))
        pearson = (y - mean.y)/sd.y
    }
    mean.log = log(lambda)
    sd.log = pi/(sqrt(3)*sigma)
    mod.pearson = (log(y) - mean.log)/sd.log
    pF1 = get(paste("p", param, sep = ""))
    quant = qnorm(pF1(y, mu = mu, sigma = sigma))
    val <- list(estimate = aa, logLik = logLik(aux), AIC = AIC(aux), 
        BIC = BIC(aux), nu = nu, pearson.res = as.vector(pearson), 
        mod.pearson.res = as.vector(mod.pearson), quant.res = c(quant), 
        convergence = aux$converged, dist="LL", param = param, param=param,
        mu.x = X, sigma.x = Z)
    class(val) <- "rregm"
	}
if(param=="LL6")
{
    X <- model.matrix(formula, data)
    Z <- model.matrix(sigma.formula, data)
    y <- model.frame(formula, data)[[1]]
      llike.LL6 <- function(theta, y, x, z, nu) {
        r1 = ncol(x)
        r2 = ncol(z)
        beta <- theta[1:r1]
        xi <- theta[r1 + 1:r2]
        mu <- exp(x %*% beta)
        sigma <- exp(z %*% xi)
        -sum(dLL6(y, mu=mu, sigma=sigma, nu=nu, log = TRUE))
    }
    aux=lm(log(y)~X[,-1,drop=FALSE])
    inits <- c(coef(aux), rep(0, ncol(Z)))
    maxi <- optim(inits, llike.LL6, y = y, x = X, z = Z, nu = nu, 
        hessian = TRUE, control = list(maxit = 1e+05))
    desv <- sqrt(diag(solve(maxi$hessian)))
    aa <- cbind(maxi$par, desv)
    rownames(aa) <- c(paste("beta_", colnames(X), sep = ""), 
        paste("nu_", colnames(Z), sep = ""))
    colnames(aa) = c("Estimate", "Std. Error")
    mu <- exp(X %*% aa[1:ncol(X), 1])
    sigma <- exp(Z %*% aa[ncol(X) + 1:ncol(Z), 1])
	lambda = mu*(1/nu-1)^(1/sigma)
   pearson=c()
if (all(sigma > 2)) {
        mean.y = lambda*(pi/sigma)/sin(pi/sigma)
        sd.y = (2*lambda^2*(pi/sigma)/sin(2*pi/sigma)-mean.y^2)
        pearson = (y - mean.y)/sd.y
    }
mean.log = log(lambda)
    sd.log = pi/(sqrt(3)*sigma)
    mod.pearson = (log(y) - mean.log)/sd.log
    quant = qnorm(pLL6(y, mu = mu, sigma = sigma, nu=nu))
    val <- list(estimate = aa, logLik = -maxi$value, AIC = 2 * 
        maxi$value + 2 * nrow(aa), BIC = 2 * maxi$value + log(length(y)) * 
        nrow(aa), nu=nu, quant.res = c(quant), 
        convergence = ifelse(maxi$convergence == 0, TRUE, FALSE), 
        dist="LL", param = param, mu.x = X, sigma.x = Z)
    class(val) <- "rregm"
}
    val
}
