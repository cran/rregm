qRLL <-
function(p, mu=1, sigma=1.5, nu=0.5, param="LL", log.p=FALSE)
{
  if (any(mu <= 0)) stop(paste("mu must be positive", "\n", ""))
  if(any(param==c("LL","LL2","LL6"))) if(any(sigma <= 0)) stop(paste("sigma must be positive", "\n", ""))
  if(any(param==c("LL3","LL4","LL5"))) if(any(sigma <= 1)) stop(paste("sigma must be greater than 1", "\n", ""))
  if (any(p <= 0) | any(p >= 1)) stop(paste("p must be between 0 and 1", "\n", ""))
  if (!any(param == c("LL", "LL2", "LL3", "LL4", "LL5","LL6"))) stop("param is not recognized")
  switch(param, LL=qLL(p, mu, sigma, log.p), 
LL2=qLL2(p, mu, sigma, log.p), 
LL3=qLL3(p, mu, sigma, log.p), 
LL4=qLL4(p, mu, sigma, log.p), 
LL5=qLL5(p, mu, sigma, log.p),
LL6=qLL6(p, mu, sigma, nu, log.p))
}
