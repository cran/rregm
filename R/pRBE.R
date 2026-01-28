pRBE <-
function(q, mu=0.5, sigma=1, param="AM", lower.tail = TRUE, log.p = FALSE)
{
  if (any(mu <= 0 | mu >= 1)) stop(paste("mu must be in (0,1)", "\n", ""))
  if(any(sigma <= 0)) stop(paste("sigma must be positive", "\n", ""))
  if (!any(param == c("AM", "GM", "HM", "MO", "MD"))) stop("param is not recognized")
  switch(param, AM=pBEAM(q, mu, sigma, lower.tail, log.p), 
GM=pBEGM(q, mu, sigma, lower.tail, log.p), 
HM=pBEHM(q, mu, sigma, lower.tail, log.p), 
MD=pBEMD(q, mu, sigma, lower.tail, log.p), 
MO=pBEMO(q, mu, sigma, lower.tail, log.p))
}
