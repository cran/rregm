qRLN<-function (p, mu = 1, sigma = 1, param = "AM", lower.tail = TRUE, 
    log.p = FALSE) 
{
    if (any(mu <= 0)) 
        stop(paste("mu must be positive", "\n", ""))
    if (any(sigma <= 0)) 
        stop(paste("sigma must be positive", "\n", ""))
    if (any(p < 0) | any(p > 1)) 
        stop(paste("p must be between 0 and 1", "\n", ""))
    if (!any(param == c("AM", "GM", "HM", "MO", "MD"))) 
        stop("param is not recognized")
    tau = switch(param, AM = 1/2, GM = 0, MD = 0, MO = -1, HM = -1/2)
    phi<-sigma^0.5
    theta=log(mu)-tau*phi
    q <- qlnorm(p, meanlog = theta, sdlog = phi, lower.tail = lower.tail, 
        log.p = log.p)
    q
}
