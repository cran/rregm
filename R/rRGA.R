rRGA <-
function (n, mu = 1, sigma = 1, param="AM") 
{
    if (any(mu <= 0)) 
        stop(paste("mu must be positive", "\n", ""))
    if (any(sigma <= 0)) 
        stop(paste("sigma must be positive", "\n", ""))
    if (any(n <= 0)) 
        stop(paste("n must be a positive integer", "\n", ""))
  if (!any(param == c("AM", "GM", "HM", "MO", "MD"))) stop("param is not recognized")
  tau=switch(param, AM=0, GM=0.5, MD=1/3, MO=1, HM=1)
aux=sigma*(sigma+4*tau)
alpha<-tau+(sqrt(aux)+sigma)/2
beta<-(sqrt(aux)+sigma)/(2*mu)
    n <- ceiling(n)
    p <- runif(n)
    r <- qgamma(p, shape=alpha, rate=beta)
    r
}
