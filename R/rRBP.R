rRBP <-
function(n, mu=1, sigma=1.5, param="AM")
{
  if (any(mu <= 0)) stop(paste("mu must be positive", "\n", ""))
  if(any(sigma <= 0)) stop(paste("sigma must be positive", "\n", ""))
    if (any(n <= 0)) 
        stop(paste("n must be a positive integer", "\n", ""))
    n <- ceiling(n)
if (!any(param == c("AM", "GM", "HM", "MO", "MD"))) 
        stop("param is not recognized")
if(param=="MO" & any( sigma<=1))  stop(paste("sigma must be greater than 1", "\n", ""))
tau1=switch(param, AM=0, GM=0.5, HM=1, MO=1, MD=1/3)
tau2=switch(param, AM=0, GM=0.5, HM=1, MO=2, MD=2/3)
  a <- mu*sigma+tau1
  b <- sigma-tau2+1   
  r <- rbetapr(n,shape1=a,shape2=b,scale=1)
  r
}
