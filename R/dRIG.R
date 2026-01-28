dRIG <-
function(x, mu=1, sigma=1.5, param="AM", log=FALSE)
{
  if (any(mu <= 0)) stop(paste("mu must be positive", "\n", ""))
  if(any(sigma <= 0)) stop(paste("sigma must be positive", "\n", ""))
  if (any(x <= 0 | x >= 1)) stop(paste("x must be in (0,1)", "\n", ""))
  if (!any(param == c("AM", "GM", "HM", "MO", "MD"))) stop("param is not recognized")
  if(param=="MO" & any(sigma <= 0)) stop(paste("sigma must be greater than 1", "\n", ""))
  switch(param, AM=dIGAM(x, mu, sigma, log), 
GM=dIGGM(x, mu, sigma, log), 
HM=dIGHM(x, mu, sigma, log), 
MD=dIGMD(x, mu, sigma, log), 
MO=dIGMO(x, mu, sigma, log))
}
