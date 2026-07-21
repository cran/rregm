pRLL <-
function(q, mu=1, sigma=1.5, nu=0.5, param="LL", lower.tail=TRUE, log.p=FALSE)
{
  if (any(mu <= 0)) stop(paste("mu must be positive", "\n", ""))
  if(any(param==c("LL","LL2","LL6"))) if(any(sigma <= 0)) stop(paste("sigma must be positive", "\n", ""))
  if(any(param==c("LL3","LL4","LL5"))) if(any(sigma <= 1)) stop(paste("sigma must be greater than 1", "\n", ""))
  if (any(q <= 0)) stop(paste("q must be positive", "\n", ""))
  if (!any(param == c("LL", "LL2", "LL3", "LL4", "LL5","LL6"))) stop("param is not recognized")
  switch(param, LL=pLL(q, mu, sigma, lower.tail, log.p), 
LL2=pLL2(q, mu, sigma, lower.tail, log.p), 
LL3=pLL3(q, mu, sigma, lower.tail, log.p), 
LL4=pLL4(q, mu, sigma, lower.tail, log.p), 
LL5=pLL5(q, mu, sigma, lower.tail, log.p),
LL6=pLL6(q, mu, sigma, nu, lower.tail, log.p))
}
