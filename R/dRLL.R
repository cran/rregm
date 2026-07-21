dRLL <-
function(x, mu=1, sigma=1.5, nu=0.5, param="LL", log=FALSE)
{
  if (any(mu <= 0)) stop(paste("mu must be positive", "\n", ""))
  if(any(param==c("LL","LL2","LL6"))) if(any(sigma <= 0)) stop(paste("sigma must be positive", "\n", ""))
  if(any(param==c("LL3","LL4","LL5"))) if(any(sigma <= 1)) stop(paste("sigma must be greater than 1", "\n", ""))
  if (any(x <= 0)) stop(paste("x must be positive", "\n", ""))
  if (!any(param == c("LL", "LL2", "LL3", "LL4", "LL5","LL6"))) stop("param is not recognized")
  switch(param, LL=dLL(x, mu, sigma, log), 
LL2=dLL2(x, mu, sigma, log), 
LL3=dLL3(x, mu, sigma, log), 
LL4=dLL4(x, mu, sigma, log), 
LL5=dLL5(x, mu, sigma, log),
LL6=dLL6(x, mu, sigma, nu, log))
}
