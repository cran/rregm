dRGA <-
function (x, mu = 1, sigma = 1, param="AM", log = FALSE) 
{
    if (any(mu <= 0)) 
        stop(paste("mu must be positive", "\n", ""))
    if (any(sigma <= 0)) 
        stop(paste("sigma must be positive", "\n", ""))
    if (any(x <= 0)) 
        stop(paste("x must be positive", "\n", ""))
  if (!any(param == c("AM", "GM", "HM", "MO", "MD"))) stop("param is not recognized")
  tau=switch(param, AM=0, GM=0.5, MD=1/3, MO=1, HM=1)
aux=sigma*(sigma+4*tau)
alpha<-tau+(sqrt(aux)+sigma)/2
beta<-(sqrt(aux)+sigma)/(2*mu)
    log.lik <- dgamma(x, shape=alpha, rate=beta, log=TRUE)
    if (log == FALSE) 
        fy <- exp(log.lik)
    else fy <- log.lik
    fy
}
