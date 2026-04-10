fit.RLN<-function (formula = formula(data), sigma.formula = ~1, data, 
    param = "AM") 
{
    if (!any(param == c("AM", "GM", "HM", "MO", "MD"))) 
        stop("param is not recognized")
    tau = switch(param, AM = 1/2, GM = 0, MD = 0, MO = -1, HM = -1/2)
    X <- model.matrix(formula, data)
    Z <- model.matrix(sigma.formula, data)
    y <- model.frame(formula, data)[[1]]
    quiet <- function(x) {
        sink(tempfile())
        on.exit(sink())
        invisible(force(x))
    }
    llike.RLN <- function(psi, y, x, z, tau) {
        r1 = ncol(x)
        r2 = ncol(z)
        beta <- psi[1:r1]
        nu <- psi[r1 + 1:r2]
        log.mu <- x %*% beta
        sigma <- exp(z %*% nu)
        phi<-sigma^0.5
        theta=log.mu-tau*phi
        -sum(dlnorm(y, meanlog = theta, sdlog = phi, log = TRUE))
    }
    aux1 <- quiet(gamlss(y ~ X[, -1], sigma.formula = ~Z[, -1], 
        data = data, family = LOGNO))
    inits <- c(aux1$mu.coefficients, aux1$sigma.coefficients)
    maxi <- optim(inits, llike.RLN, y = y, x = X, z = Z, tau = tau, 
        hessian = TRUE, control = list(maxit = 1e+05))
    desv <- sqrt(diag(solve(maxi$hessian)))
    aa <- cbind(maxi$par, desv)
    rownames(aa) <- c(paste("beta_", colnames(X), sep = ""), 
        paste("nu_", colnames(Z), sep = ""))
    colnames(aa) = c("Estimate", "Std. Error")
    log.mu <- X %*% aa[1:ncol(X), 1]
    sigma <- exp(Z %*% aa[ncol(X) + 1:ncol(Z), 1])
    phi<-sigma^0.5
    theta=log.mu-tau*phi

    mean.log <- theta
    sd.log <- phi
    mean.y <- exp(theta+phi^2/2)
	sd.y <- sqrt((exp(phi^2)-1)*exp(2*theta+phi^2))
    pearson <- (y - mean.y)/sd.y
    mod.pearson <- (log(y) - mean.log)/sd.log
    quant = (log(y) - theta)/phi
    val <- list(estimate = aa, logLik = -maxi$value, AIC = 2 * 
        maxi$value + 2 * nrow(aa), BIC = 2 * maxi$value + log(length(y)) * 
        nrow(aa), tau = tau, pearson.res = as.vector(pearson), 
        mod.pearson.res = as.vector(mod.pearson), quant.res = c(quant), 
        convergence = ifelse(maxi$convergence == 0, TRUE, FALSE), 
        dist = "LN", param = param, mu.x = X, sigma.x = Z)
    class(val) <- "rregm"
    val
}
