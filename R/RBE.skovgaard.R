RBE.skovgaard <- function(formula, data, reduced.fo=~1, phi.test=NULL, param="AM")
{
if (!inherits(formula, "formula")) {
        if (inherits(formula, "data.frame")) 
            warning("You gave a data.frame instead of a formula.")
        stop("formula is not an object of type formula")
    }
    if (!inherits(data, "data.frame")) {
        if (inherits(data, "formula")) 
            warning("You gave a formula instead of a data.frame.")
        stop("data is not an object of type data.frame")
    }
    if (missing(formula)) 
        stop("Missing formula")
    if (missing(data)) 
        stop("Missing data")
y <- model.frame(formula, data)[,1]
X <- model.matrix(formula, data)
X0 <- matrix(1, ncol=length(y))
if(reduced.fo!=~1){X0 <- model.matrix(reduced.fo)}
fit.RBE2<-function (formula = formula(data), sigma.formula = ~1, data, 
    param = "AM", phi.fixed=FALSE) 
{
    if (!any(param == c("AM", "GM", "HM", "MO", "MD"))) 
        stop("param is not recognized")
    quiet <- function(x) {
        sink(tempfile())
        on.exit(sink())
        invisible(force(x))
    }
    family.aux = paste("BE", param, sep = "")
    if(!phi.fixed)
    {
	    aux = gamlss(formula, sigma.formula = sigma.formula, family = family.aux, data=data,control=gamlss::gamlss.control(trace = FALSE))
	    #aa = quiet(summary(aux)[, 1:2])
		aa=matrix(c(aux$mu.coefficients, aux$sigma.coefficients), ncol=1)
	    #rownames(aa) <- c(paste("beta_", rownames(aa)[1:length(aux$mu.coefficients)], 
	    #    sep = ""), paste("nu_", rownames(aa)[length(aux$mu.coefficients) + 
          #	  1:length(aux$sigma.coefficients)], sep = ""))
	    tau1 = switch(param, AM = 0, GM = 0.5, MD = 1/3, MO = 1, 
	        HM = 1)
	    tau2 = switch(param, AM = 0, GM = 0.5, MD = 2/3, MO = 2, 
	        HM = 1)
	    g1 = aux$mu.link
	    mstats <- checklink(g1, family.aux, g1, c("logit", "probit", 
	        "cloglog", "cauchit", "log", "own"))
	    g1 = mstats$linkinv
	    g2 = aux$sigma.link
	    mstats <- checklink(g2, family.aux, g2, c("log", "inverse", 
	        "identity", "own"))
	    g2 = mstats$linkinv
	    mu = g1(aux$mu.lp)
	    sigma = g2(aux$sigma.lp)
	    alpha = mu * sigma + tau1
	    beta = (1 - mu) * sigma + tau2 - tau1
	    y <- model.frame(formula, data)[[1]]
	    mean.y = alpha/(alpha + beta)
 	   sd.y = sqrt(alpha * beta/((alpha + beta)^2 * (alpha + beta + 
	        1)))
	    pearson = (y - mean.y)/sd.y
	    mean.log = digamma(alpha) - digamma(beta)
	    sd.log = sqrt(trigamma(alpha) + trigamma(beta))
	    mod.pearson = (log(y/(1 - y)) - mean.log)/sd.log
	    pF1 = get(paste("p", family.aux, sep = ""))
	    quant = qnorm(pF1(y, mu = mu, sigma = sigma))
	    val <- list(estimate = aa, logLik = logLik(aux), AIC = AIC(aux), 
      	  BIC = BIC(aux), tau1 = tau1, tau2 = tau2, pearson.res = as.vector(pearson), 
	        mod.pearson.res = as.vector(mod.pearson), quant.res = c(quant), 
      	  convergence = aux$converged, dist = "BE", param = param, 
	        mu.x = aux$mu.x, sigma.x = aux$sigma.x)
	}
    if(is.numeric(phi.fixed))
    {
	    aux = gamlss(formula, family = family.aux, sigma.fix=TRUE, sigma.start=phi.fixed, data=data,control=gamlss::gamlss.control(trace = FALSE))
	    #aa = quiet(summary(aux)[, 1:2])
		aa=matrix(coef(aux), ncol=1)
	   # aa = aa[-nrow(aa), 1, drop=FALSE]
	   # rownames(aa) <- c(paste("beta_", rownames(aa)[1:length(aux$mu.coefficients)], 
	    #    sep = ""))
	    tau1 = switch(param, AM = 0, GM = 0.5, MD = 1/3, MO = 1, 
	        HM = 1)
	    tau2 = switch(param, AM = 0, GM = 0.5, MD = 2/3, MO = 2, 
	        HM = 1)
	    g1 = aux$mu.link
	    mstats <- checklink(g1, family.aux, g1, c("logit", "probit", 
	        "cloglog", "cauchit", "log", "own"))
	    g1 = mstats$linkinv
	    mu = g1(aux$mu.lp)
	    sigma = phi.fixed
	    alpha = mu * sigma + tau1
	    beta = (1 - mu) * sigma + tau2 - tau1
	    y <- model.frame(formula, data)[[1]]
	    mean.y = alpha/(alpha + beta)
 	   sd.y = sqrt(alpha * beta/((alpha + beta)^2 * (alpha + beta + 
	        1)))
	    pearson = (y - mean.y)/sd.y
	    mean.log = digamma(alpha) - digamma(beta)
	    sd.log = sqrt(trigamma(alpha) + trigamma(beta))
	    mod.pearson = (log(y/(1 - y)) - mean.log)/sd.log
	    pF1 = get(paste("p", family.aux, sep = ""))
	    quant = qnorm(pF1(y, mu = mu, sigma = sigma))
	    val <- list(estimate = aa, logLik = logLik(aux), AIC = AIC(aux), 
      	  BIC = BIC(aux), tau1 = tau1, tau2 = tau2, pearson.res = as.vector(pearson), 
	        mod.pearson.res = as.vector(mod.pearson), quant.res = c(quant), 
      	  convergence = aux$converged, dist = "BE", param = param, 
	        mu.x = aux$mu.x, sigma.x = aux$sigma.x)
	}
    class(val) <- "rregm"
    val
}
solve2 <- function(A) {
    chol2inv(chol(A))
  }
  c1 = switch(param, AM = 0, GM = 0.5, MD = 1/3, MO = 1, HM = 1)
  c2 = switch(param, AM = 0, GM = 0.5, MD = 2/3, MO = 2, HM = 1)

  n = length(y)
  Z = matrix(1, nrow=n)

  p1 = ncol(X)
  p2 = 1
  p10 = ncol(X0)
  p20 = ifelse(is.null(phi.test), 1, 0)

  p  = p1 + p2
  p0 = p10 + p20
  q  = p - p0

  nomesX  = colnames(X)
  nomesX0 = colnames(X0)

  sel = which(nomesX %in% nomesX0)
  if(is.null(phi.test)) sel = c(sel, p1+1)

  phi.aux = ifelse(is.null(phi.test), FALSE, phi.test)

  dat  <- data.frame(y=y, X[,-1,drop=FALSE])
  dat0 <- data.frame(y=y, X0[,-1,drop=FALSE])

  form  <- as.formula(paste("y ~", paste(names(dat)[-1], collapse="+")))
  form0 <- as.formula(paste("y ~", paste(names(dat0)[-1], collapse="+")))

  fit   <- fit.RBE2(form, sigma.formula=~1, data=dat, param=param)
  fit.0 <- fit.RBE2(form0, data=dat0, param=param, phi.fixed=phi.aux)

  theta   = coef(fit)
  theta.0 = coef(fit.0)

  beta   = theta[1:p1]
  beta.0 = theta.0[1:p10]

  mu    = plogis(X  %*% beta)
  mu.0  = plogis(X0 %*% beta.0)

  lambda = theta[p1+1]
  phi    = exp(lambda)

  if(is.null(phi.test)) {
    lambda.0 = theta.0[p10+1]
    phi.0 = exp(lambda.0)
  } else {
    phi.0 = phi.test
  }
  P = matrix(0, nrow=2*n, ncol=p1+1)
  P[1:n,1:p1] = X
  P[(n+1):(2*n), p1+1] = 1

  ones = rep(1,n)
  zero = matrix(0,n,n)
  dmu = as.vector(mu*(1-mu))
  d2mu = as.vector(mu*(1-mu)*(1-2*mu))

  Vmu = diag(dmu)

  y.star  = log(y) - log1p(-y)
  y.cross = log1p(-y)

  mu.cross = digamma((1-mu)*phi+c2-c1) - digamma(phi+c2)
  mu.star  = digamma(mu*phi+c1) - digamma(phi+c2) - mu.cross

  sss = trigamma(mu*phi+c1)+trigamma((1-mu)*phi+c2-c1)
  scc = trigamma((1-mu)*phi+c2-c1)-trigamma(phi+c2)
  ssc = -trigamma((1-mu)*phi+c2-c1)

  wbb = as.vector(phi^2*sss*(dmu^2))
  wbp = as.vector(phi*(mu*sss+ssc)*dmu)
  wpp = as.vector(mu*(mu*sss+2*ssc)+scc)

  wbb2 = as.vector(-phi*(y.star-mu.star)*d2mu)
  wbp2 = as.vector(-(y.star-mu.star)*dmu)

  W1 = rbind(cbind(diag(wbb), diag(wbp)),
             cbind(diag(wbp), diag(wpp)))

  W2 = rbind(cbind(diag(wbb2), diag(wbp2)),
             cbind(diag(wbp2), diag(0,n)))

  Ubeta = phi * t(X) %*% Vmu %*% (y.star - mu.star)
  Uphi  = sum(mu*(y.star-mu.star)+(y.cross-mu.cross))

  U.hat = c(Ubeta, Uphi)

  J.hat = t(P) %*% (W1+W2) %*% P
  K.hat = t(P) %*% W1 %*% P

  dmu0  = as.vector(mu.0*(1-mu.0))
  d2mu0 = as.vector(mu.0*(1-mu.0)*(1-2*mu.0))

  Vmu0 = diag(dmu0)

  mu.cross0 = digamma((1-mu.0)*phi.0+c2-c1) - digamma(phi.0+c2)
  mu.star0  = digamma(mu.0*phi.0+c1) - digamma(phi.0+c2) - mu.cross0

  sss0 = trigamma(mu.0*phi.0+c1)+trigamma((1-mu.0)*phi.0+c2-c1)
  scc0 = trigamma((1-mu.0)*phi.0+c2-c1)-trigamma(phi.0+c2)
  ssc0 = -trigamma((1-mu.0)*phi.0+c2-c1)

  wbb0 = as.vector(phi.0^2*sss0*(dmu0^2))
  wbp0 = as.vector(phi.0*(mu.0*sss0+ssc0)*dmu0)
  wpp0 = as.vector(mu.0*(mu.0*sss0+2*ssc0)+scc0)

  wbb20 = as.vector(-phi.0*(y.star-mu.star0)*d2mu0)
  wbp20 = as.vector(-(y.star-mu.star0)*dmu0)

  W1 = rbind(cbind(diag(wbb0), diag(wbp0)),
             cbind(diag(wbp0), diag(wpp0)))

  W2 = rbind(cbind(diag(wbb20), diag(wbp20)),
             cbind(diag(wbp20), diag(0,n)))

  Ubeta0 = phi.0 * t(X) %*% Vmu0 %*% (y.star - mu.star0)
  Uphi0  = sum(mu.0*(y.star-mu.star0)+(y.cross-mu.cross0))

  U.tilde = c(Ubeta0, Uphi0)

  J.tilde = t(P) %*% (W1+W2) %*% P
  K.tilde = t(P) %*% W1 %*% P

  d1 = phi*((mu*phi-mu.0*phi.0)*sss + (phi-phi.0)*ssc)
  d2 = (mu*sss+ssc)*(mu*phi-mu.0*phi.0) + (mu*ssc+scc)*(phi-phi.0)

  q.vec = c(d1 * dmu, d2)
  q.bar = t(P) %*% q.vec

  m1 = as.vector(phi*phi.0*sss)
  m2 = as.vector(phi*(mu.0*sss+ssc))
  m3 = as.vector(phi.0*(mu*sss+ssc))
  m4 = as.vector(mu*mu.0*sss+(mu+mu.0)*ssc+scc)

  Vhat  = rbind(cbind(diag(dmu), zero), cbind(zero, diag(n)))
  Vtil  = rbind(cbind(diag(dmu0), zero), cbind(zero, diag(n)))

  M = rbind(cbind(diag(m1), diag(m2)),
            cbind(diag(m3), diag(m4)))

  Upsilon = t(P) %*% Vhat %*% M %*% Vtil %*% P
  solve_safe<-skewMLRM::solve2
  J11 = J.tilde[-sel, -sel, drop=FALSE]

  SLR = 2*(logLik(fit) - logLik(fit.0))

  term1 = sqrt(det(K.tilde)*det(K.hat)*det(J11))

  term2 = abs(t(U.tilde) %*% solve_safe(Upsilon) %*%
              K.hat %*% solve_safe(J.hat) %*%
              Upsilon %*% solve_safe(K.tilde) %*% U.tilde)^(q/2)

  term3 = abs(det(Upsilon)) *
          abs(det((K.tilde %*% solve_safe(Upsilon) %*%
                   J.hat %*% solve_safe(K.hat) %*%
                   Upsilon)[-sel,-sel,drop=FALSE]))^(1/2)

  term4 = SLR^(q/2 - 1) *
          abs(t(U.tilde) %*% solve_safe(Upsilon) %*% q.bar)

  xi = (term1/term4)*(term2/term3)

  SLR.sk1 = SLR
  SLR.sk2 = SLR

  if(SLR > 0.1 && is.finite(xi) && xi > 0){
    SLR.sk1 = SLR - 2*log(xi)
    SLR.sk2 = SLR*(1 - log(xi)/SLR)^2
  }
  stat=c(SLR, SLR.sk1, SLR.sk2)
  names(stat)=c("SLR","SLR1","SLR2")
  p.value=pchisq(stat, df=ncol(X)-ncol(X0), lower.tail=FALSE)
  results=list(statistic=stat, p.value=p.value, y=y, X=X, X0=X0, param=param)
  class(results)="LRskov"
  return(results)
}

