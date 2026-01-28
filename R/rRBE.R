rRBE <-
function(n, mu=0.5, sigma=1, param="AM")
{
  if (any(mu <= 0 | mu >= 1)) stop(paste("mu must be in (0,1)", "\n", ""))
  if(any(sigma <= 0)) stop(paste("sigma must be positive", "\n", ""))
    if (any(n <= 0)) 
        stop(paste("n must be a positive integer", "\n", ""))
    n <- ceiling(n)
  if (!any(param == c("AM", "GM", "HM", "MO", "MD"))) stop("param is not recognized")
  switch(param, AM=rBEAM(n, mu, sigma), 
GM=rBEGM(n, mu, sigma), 
HM=rBEHM(n, mu, sigma), 
MD=rBEMD(n, mu, sigma), 
MO=rBEMO(n, mu, sigma))
}
