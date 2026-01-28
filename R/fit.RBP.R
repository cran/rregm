fit.RBP <-
function(formula = formula(data), sigma.formula=~1, data, param="AM")
{
if (!any(param == c("AM", "GM", "HM", "MO", "MD"))) stop("param is not recognized")
quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 
E.step<-function(zeta, data, param="AM"){
tau1=switch(param, AM=0, GM=0.5, HM=1, MO=1, MD=1/3)
tau2=switch(param, AM=0, GM=0.5, HM=1, MO=2, MD=2/3)
y=data$y;x=data$x;z=data$z
p=ncol(x);q=ncol(z)
beta<-zeta[1:p]
nu<-zeta[(p+1):(p+q)]
mu<-exp(x%*%beta)
sigma<-max(0,tau2-1)+exp(z%*%nu)
a <- mu*sigma+tau1
b <- sigma-tau2+1    
logw=log1p(y)-digamma(a+b)
w1=(a+b)/(1+y)
list(w1=w1, logw=logw)}
aux.M.step<-function(zeta,data,w1,logw,param){
tau1=switch(param, AM=0, GM=0.5, HM=1, MO=1, MD=1/3)
tau2=switch(param, AM=0, GM=0.5, HM=1, MO=2, MD=2/3)
y=data$y;x=data$x;z=data$z
p=ncol(x);q=ncol(z)
beta<-zeta[1:p]
nu<-zeta[(p+1):(p+q)]
mu<-exp(x%*%beta)
sigma<-max(0,tau2-1)+exp(z%*%nu)
a <- mu*sigma+tau1
b <- sigma-tau2+1 
-sum(-lgamma(a)-lgamma(b)-(a+b)*logw-w1*(y+1)+(a-1)*log(y))
}
M.step<-function(zeta,data,w1=w1,logw=logw,param=param) nlminb(zeta,aux.M.step,data=data,w1=w1,logw=logw,param=param)$par
llike.BP<-function(zeta,data,param){
y=data$y;x=data$x;z=data$z
if (!any(param == c("AM", "GM", "HM", "MO", "MD"))) 
        stop("param is not recognized")
tau1=switch(param, AM=0, GM=0.5, HM=1, MO=1, MD=1/3)
tau2=switch(param, AM=0, GM=0.5, HM=1, MO=2, MD=2/3)
p=ncol(x);q=ncol(z)
beta<-zeta[1:p]
nu<-zeta[p+1:q]
mu<-exp(x%*%beta)
sigma<-max(0,tau2-1)+exp(z%*%nu)
-sum(dRBP(y,mu,sigma,param, log=TRUE))}
x <- model.matrix(formula, data)
z <- model.matrix(sigma.formula, data)
y <- model.frame(formula, data)[[1]]
data.aux=list(y=y, x=x, z=z)
p=ncol(x);q=ncol(z)
aa=lm(log(y)~x[,-1,drop=FALSE])
zeta=c(coef(aa),log(sigma(aa)),rep(0,ncol(z)-1))
i=1;dif=1
prec=1e-4; max.iter=1000
while(dif>prec && i<=max.iter)
{
aux<-E.step(zeta,data.aux,param)
w1=aux$w1; logw=aux$logw
zeta.new<-M.step(zeta,data.aux,w1=w1,logw=logw,param=param)
dif<-max(abs(zeta.new-zeta))
zeta<-zeta.new
i=i+1
}
convergence=ifelse(dif>prec || i>max.iter,FALSE,TRUE)
se=sqrt(diag(solve(hessian(llike.BP, x0=zeta, data=data.aux, param=param))))
aa=cbind(zeta,se)
llike=-llike.BP(zeta, data.aux, param)
rownames(aa)<-c(paste("beta_",colnames(x),sep=""),
paste("nu_",colnames(z),sep=""))
colnames(aa)=c("Estimate","Std. Error")
tau1=switch(param, AM=0, GM=0.5, HM=1, MO=1, MD=1/3)
tau2=switch(param, AM=0, GM=0.5, HM=1, MO=2, MD=2/3)
mu<-exp(x%*%aa[1:p,1])
sigma<-max(0,tau2-1)+exp(z%*%aa[p+1:q,1])
alpha=mu*sigma+tau1
beta=sigma-tau2+1
pearson=NULL
if(all(beta>2))
{
mean.y=alpha/(beta-1)
sd.y=sqrt(alpha*(alpha+beta-1)/((beta-2)*(beta-1)^2))
pearson=(y-mean.y)/sd.y
}
##mod. pearson based on log(Y/(1+Y))
mean.log=digamma(alpha)-digamma(alpha+beta)
sd.log=sqrt(trigamma(alpha)-trigamma(alpha+beta))
mod.pearson=(log(y/(1+y))-mean.log)/sd.log
quant=qnorm(pRBP(y, mu=mu, sigma=sigma, param=param))
val=list(estimate=aa, logLik=llike, AIC=-2*llike+2*(p+q), BIC=-2*llike+log(length(y))*(p+q), tau1=tau1, tau2=tau2, 
pearson.res=as.vector(pearson), mod.pearson.res=as.vector(mod.pearson), quant.res=c(quant), convergence=convergence, dist="BP",param=param,
mu.x=x, sigma.x=z)
class(val) <- "rregm"
val
}
