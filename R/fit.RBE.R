fit.RBE <-
function(formula = formula(data), sigma.formula=~1, data, param="AM")
{
  if (!any(param == c("AM", "GM", "HM", "MO", "MD"))) stop("param is not recognized")
quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 
family.aux=paste("BE",param,sep="")
aux=gamlss(formula, sigma.formula=sigma.formula, family=family.aux)
aa=summary(aux)[,1:2]
rownames(aa)<-c(paste("beta_",rownames(aa)[1:length(aux$mu.coefficients)],sep=""),
paste("nu_",rownames(aa)[length(aux$mu.coefficients)+1:length(aux$sigma.coefficients)],sep=""))
  tau1=switch(param, AM=0, GM=0.5, MD=1/3, MO=1, HM=1)
  tau2=switch(param, AM=0, GM=0.5, MD=2/3, MO=2, HM=1)
g1=aux$mu.link
mstats <- checklink(g1, family.aux, g1, 
        c("logit", "probit", "cloglog", "cauchit", "log", "own"))
g1=mstats$linkinv
g2=aux$sigma.link
mstats <- checklink(g2, family.aux, g2, 
        c("log", "inverse", "identity", "own"))
g2=mstats$linkinv
mu=g1(aux$mu.lp)
sigma=g2(aux$sigma.lp)
alpha=mu*sigma+tau1
beta=(1-mu)*sigma+tau2-tau1
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
val<-list(estimate=aa, logLik=logLik(aux), AIC=AIC(aux), BIC=BIC(aux), tau1=tau1, tau2=tau2, 
pearson.res=as.vector(pearson), mod.pearson.res=as.vector(mod.pearson), quant.res=c(quant), convergence=aux$converged,
dist="BE",param=param, mu.x=aux$mu.x, sigma.x=aux$sigma.x)
class(val) <- "rregm"
val
}
