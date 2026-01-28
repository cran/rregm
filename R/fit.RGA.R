fit.RGA <-
function(formula = formula(data), sigma.formula=~1, data, param="AM")
{  
  if (!any(param == c("AM", "GM", "HM", "MO", "MD"))) stop("param is not recognized")
  tau=switch(param, AM=0, GM=0.5, MD=1/3, MO=1, HM=1)
X <- model.matrix(formula, data)
Z <- model.matrix(sigma.formula, data)
y <- model.frame(formula, data)[[1]]
quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 
llike.RGA<-function(theta, y, x, z, tau)
{
r1=ncol(x)
r2=ncol(z)
beta<-theta[1:r1]
nu<-theta[r1+1:r2]
mu<-exp(x%*%beta)
sigma<-exp(z%*%nu)
aux<-sigma*(sigma+4*tau)
c1=(sqrt(aux)+sigma)/2
alpha=tau+c1
beta=c1/mu
-sum(dgamma(y, shape=alpha, rate=beta, log=TRUE))
}
aux1<-quiet(gamlss(y~X[,-1], sigma.formula=~Z[,-1], data=data, family=GA))
inits<-c(aux1$mu.coefficients, aux1$sigma.coefficients)

maxi<-optim(inits, llike.RGA, y=y, x=X, z=Z, tau=tau, 
hessian=TRUE, control=list(maxit=100000))
desv<-sqrt(diag(solve(maxi$hessian)))
aa<-cbind(maxi$par, desv)
rownames(aa)<-c(paste("beta_",colnames(X),sep=""),
paste("nu_",colnames(Z),sep=""))
colnames(aa)=c("Estimate","Std. Error")
mu<-exp(X%*%aa[1:ncol(X),1])
sigma<-exp(Z%*%aa[ncol(X)+1:ncol(Z),1])
aux<-sigma*(sigma+4*tau)
c1=(sqrt(aux)+sigma)/2
alpha=tau+c1
beta=c1/mu
mean.log<-digamma(alpha)-log(beta)
sd.log<-sqrt(trigamma(alpha))
pearson<-(y-alpha/beta)/sqrt(alpha/beta^2)
mod.pearson<-(log(y)-mean.log)/sd.log
quant=qnorm(pRGA(y, mu=mu, sigma=sigma, param=param))
val<-list(estimate=aa, logLik=-maxi$value, AIC=2*maxi$value+2*nrow(aa), BIC=2*maxi$value+log(length(y))*nrow(aa), tau=tau, pearson.res=as.vector(pearson), mod.pearson.res=as.vector(mod.pearson),
quant.res=c(quant), convergence=ifelse(maxi$convergence==0, TRUE, FALSE), dist="GA",param=param, mu.x=X, sigma.x=Z)
class(val) <- "rregm"
val
}
