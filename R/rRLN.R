rRLN<-function (n, mu = 1, sigma = 1, param = "AM") 
{
    if (any(mu <= 0)) 
        stop(paste("mu must be positive", "\n", ""))
    if (any(sigma <= 0)) 
        stop(paste("sigma must be positive", "\n", ""))
    if (any(n <= 0)) 
        stop(paste("n must be a positive integer", "\n", ""))
    if (!any(param == c("AM", "GM", "HM", "MO", "MD"))) 
        stop("param is not recognized")
    tau = switch(param, AM = 1/2, GM = 0, MD = 0, MO = -1, HM = -1/2)
    phi<-sigma^0.5
    theta=log(mu)-tau*phi
    n <- ceiling(n)
    p <- runif(n)
    r <- qlnorm(p, meanlog = theta, sdlog = phi)
    r
}