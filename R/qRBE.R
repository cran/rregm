qRBE <-
function(p, mu=0.5, sigma=1, param="AM", lower.tail = TRUE, log.p = FALSE)
{
  if (any(mu <= 0 | mu >= 1)) stop(paste("mu must be in (0,1)", "\n", ""))
  if(any(sigma <= 0)) stop(paste("sigma must be positive", "\n", ""))
  if (any(p <= 0) | any(p >= 1)) stop(paste("p must be between 0 and 1", "\n", ""))
  if (!any(param == c("AM", "GM", "HM", "MO", "MD"))) stop("param is not recognized")
  switch(param, AM=qBEAM(p, mu, sigma, lower.tail, log.p), 
GM=qBEGM(p, mu, sigma, lower.tail, log.p), 
HM=qBEHM(p, mu, sigma, lower.tail, log.p), 
MD=qBEMD(p, mu, sigma, lower.tail, log.p), 
MO=qBEMO(p, mu, sigma, lower.tail, log.p))
}
