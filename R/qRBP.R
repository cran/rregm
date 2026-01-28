qRBP <-
function(p, mu=1, sigma=1.5, param="AM", lower.tail = TRUE, log.p = FALSE)
{
  if (any(mu <= 0)) stop(paste("mu must be positive", "\n", ""))
  if(any(sigma <= 0)) stop(paste("sigma must be positive", "\n", ""))
  if (any(p <= 0) | any(p >= 1)) stop(paste("p must be between 0 and 1", "\n", ""))
if(param=="MO" & any( sigma<=1))  stop(paste("sigma must be greater than 1", "\n", ""))
tau1=switch(param, AM=0, GM=0.5, HM=1, MO=1, MD=1/3)
tau2=switch(param, AM=0, GM=0.5, HM=1, MO=2, MD=2/3)
  a <- mu*sigma+tau1
  b <- sigma-tau2+1    
  q <- qbetapr(p, shape1 = a, shape2 = b,scale=1, lower.tail = lower.tail, log.p = log.p)
  q

}
