qRGA <-
function (p, mu = 1, sigma = 1, param = "AM", lower.tail = TRUE, log.p = FALSE) 
{
    if (any(mu <= 0)) 
        stop(paste("mu must be positive", "\n", ""))
    if (any(sigma <= 0)) 
        stop(paste("sigma must be positive", "\n", ""))
    if (any(p < 0) | any(p > 1)) 
        stop(paste("p must be between 0 and 1", "\n", ""))
  if (!any(param == c("AM", "GM", "HM", "MO", "MD"))) stop("param is not recognized")
  tau=switch(param, AM=0, GM=0.5, MD=1/3, MO=1, HM=1)
aux=sigma*(sigma+4*tau)
alpha<-tau+(sqrt(aux)+sigma)/2
beta<-(sqrt(aux)+sigma)/(2*mu)
    q <- qgamma(p, shape = alpha, rate = beta, lower.tail = lower.tail, 
        log.p = log.p)
    q
}
