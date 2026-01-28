rRIG <-
function(n, mu=1, sigma=1.5, param="AM")
{
  if (any(mu <= 0)) stop(paste("mu must be positive", "\n", ""))
  if(any(sigma <= 0)) stop(paste("sigma must be positive", "\n", ""))
  if(param=="MO" & any(sigma <= 0)) stop(paste("sigma must be greater than 1", "\n", ""))
    if (any(n <= 0)) 
        stop(paste("n must be a positive integer", "\n", ""))
    n <- ceiling(n)
  if (!any(param == c("AM", "GM", "HM", "MO", "MD"))) stop("param is not recognized")
  switch(param, AM=rIGAM(n, mu, sigma), 
GM=rIGGM(n, mu, sigma), 
HM=rIGHM(n, mu, sigma), 
MD=rIGMD(n, mu, sigma), 
MO=rIGMO(n, mu, sigma))
}
