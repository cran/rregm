dRLN<-function (x, mu = 1, sigma = 1, param = "AM", log = FALSE) 
{
    if (any(mu <= 0)) 
        stop(paste("mu must be positive", "\n", ""))
    if (any(sigma <= 0)) 
        stop(paste("sigma must be positive", "\n", ""))
    if (any(x <= 0)) 
        stop(paste("x must be positive", "\n", ""))
    if (!any(param == c("AM", "GM", "HM", "MO", "MD"))) 
        stop("param is not recognized")
    tau = switch(param, AM = 1/2, GM = 0, MD = 0, MO = -1, HM = -1/2)
    phi<-sigma^0.5
    theta=log(mu)-tau*phi
    log.lik<-dlnorm(y, meanlog = theta, sdlog = phi, log = TRUE)
    if (log == FALSE) 
        fy <- exp(log.lik)
    else fy <- log.lik
    fy
}