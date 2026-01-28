pRIG <-
function(q, mu=1, sigma=1.5, param="AM", lower.tail = TRUE, log.p = FALSE)
{
  if (any(mu <= 0)) stop(paste("mu must be positive", "\n", ""))
  if(any(sigma <= 0)) stop(paste("sigma must be positive", "\n", ""))
  if (!any(param == c("AM", "GM", "HM", "MO", "MD"))) stop("param is not recognized")
  if(param=="MO" & any(sigma <= 0)) stop(paste("sigma must be greater than 1", "\n", ""))
  switch(param, AM=pIGAM(q, mu, sigma, lower.tail, log.p), 
GM=pIGGM(q, mu, sigma, lower.tail, log.p), 
HM=pIGHM(q, mu, sigma, lower.tail, log.p), 
MD=pIGMD(q, mu, sigma, lower.tail, log.p), 
MO=pIGMO(q, mu, sigma, lower.tail, log.p))
}
