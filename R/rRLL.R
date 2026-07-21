rRLL <-
function(n, mu=1, sigma=1.5, nu=0.5, param="LL")
{
  if (any(mu <= 0)) stop(paste("mu must be positive", "\n", ""))
  if(any(param==c("LL","LL2","LL6"))) if(any(sigma <= 0)) stop(paste("sigma must be positive", "\n", ""))
  if(any(param==c("LL3","LL4","LL5"))) if(any(sigma <= 1)) stop(paste("sigma must be greater than 1", "\n", ""))
  if (any(nu <= 0 | nu>=1)) stop(paste("nu must be in (0,1)", "\n", ""))
    if (any(n <= 0)) 
        stop(paste("n must be a positive integer", "\n", ""))
    n <- ceiling(n)
  if (!any(param == c("LL", "LL2", "LL3", "LL4", "LL5","LL6"))) stop("param is not recognized")
  switch(param, LL=rLL(n, mu, sigma), 
LL2=rLL2(n, mu, sigma), 
LL3=rLL3(n, mu, sigma), 
LL4=rLL4(n, mu, sigma), 
LL5=rLL5(n, mu, sigma),
LL6=rLL6(n, mu, sigma, nu))
}
