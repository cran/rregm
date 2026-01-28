dRBE <-
function(x, mu=0.5, sigma=1, param="AM", log=FALSE)
{
  if (any(mu <= 0 | mu >= 1)) stop(paste("mu must be in (0,1)", "\n", ""))
  if(any(sigma <= 0)) stop(paste("sigma must be positive", "\n", ""))
  if (any(x <= 0 | x >= 1)) stop(paste("x must be in (0,1)", "\n", ""))
  if (!any(param == c("AM", "GM", "HM", "MO", "MD"))) stop("param is not recognized")
  switch(param, AM=dBEAM(x, mu, sigma, log), 
GM=dBEGM(x, mu, sigma, log), 
HM=dBEHM(x, mu, sigma, log), 
MD=dBEMD(x, mu, sigma, log), 
MO=dBEMO(x, mu, sigma, log))
}
