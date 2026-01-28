qRIG <-
function(p, mu=1, sigma=1.5, param="AM", lower.tail = TRUE, log.p = FALSE)
{
  if (any(mu <= 0)) stop(paste("mu must be positive", "\n", ""))
  if(any(sigma <= 0)) stop(paste("sigma must be positive", "\n", ""))
  if (any(p <= 0) | any(p >= 1)) stop(paste("p must be between 0 and 1", "\n", ""))
  if (!any(param == c("AM", "GM", "HM", "MO", "MD"))) stop("param is not recognized")
  if(param=="MO" & any(sigma <= 0)) stop(paste("sigma must be greater than 1", "\n", ""))
  switch(param, AM=qIGAM(p, mu, sigma, lower.tail, log.p), 
GM=qIGGM(p, mu, sigma, lower.tail, log.p), 
HM=qIGHM(p, mu, sigma, lower.tail, log.p), 
MD=qIGMD(p, mu, sigma, lower.tail, log.p), 
MO=qIGMO(p, mu, sigma, lower.tail, log.p))
}
