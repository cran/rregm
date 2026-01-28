fit.RIG <-
function(formula = formula(data), sigma.formula=~1, data, param="AM")
{
  if (!any(param == c("AM", "GM", "HM", "MO", "MD"))) stop("param is not recognized")
family.aux=paste("IG",param,sep="")
aux=gamlss(formula, sigma.formula=sigma.formula, family=family.aux)
aa=summary(aux)[,1:2]
rownames(aa)<-c(paste("beta_",rownames(aa)[1:length(aux$mu.coefficients)],sep=""),
paste("nu_",rownames(aa)[length(aux$mu.coefficients)+1:length(aux$sigma.coefficients)],sep=""))
tau=switch(param, AM=1, GM=0.5, MD=1/3, MO=-1, HM=0)
g1=aux$mu.link
mstats <- checklink(g1, family.aux, g1, 
        c("inverse", "log", "identity", "own"))
g1=mstats$linkinv
g2=aux$sigma.link
mstats <- checklink(g2, family.aux, g2, 
        c("log", "inverse", "identity", "own","logshiftto1"))
g2=mstats$linkinv
mu=g1(aux$mu.lp)
sigma=g2(aux$sigma.lp)
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
val<-list(estimate=aa, logLik=logLik(aux), AIC=AIC(aux), BIC=BIC(aux), tau=tau, 
pearson.res=as.vector(pearson), mod.pearson.res=as.vector(mod.pearson), 
quant.res=c(quant), convergence=aux$converged, dist="IG",param=param, mu.x=aux$mu.x, sigma.x=aux$sigma.x)
class(val) <- "rregm"
val
}
