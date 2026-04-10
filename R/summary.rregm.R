summary.rregm = function (object, ...) 
{
    asterisk <- function(x) {
        if (x > 0.1) {
            ast = " "
        }
        else {
            if (x > 0.05) {
                ast = "."
            }
            else {
                if (x > 0.01) {
                  ast = "*"
                }
                else {
                  if (x > 0.001) {
                    ast = "**"
                  }
                  else {
                    {
                      ast = "***"
                    }
                  }
                }
            }
        }
        return(ast)
    }
    tt <- cbind(object$estimate[, 1], object$estimate[, 2], object$estimate[, 
        1]/object$estimate[, 2], pnorm(abs(object$estimate[, 
        1]/object$estimate[, 2]), lower.tail = FALSE))
    ast = sapply(tt[, 4], FUN = asterisk)
	round2=function(x,digits){ifelse(round(x,digits)<0.0001,"<0.0001",round(x,digits))}
    tt = data.frame(round(tt[,1:3], 4),round2(tt[,4], 4), ast)
    colnames(tt) <- c("coef", "s.e.", "z value", "Pr(>|z|)", 
        "")
    tipo = switch(object$param, AM = "mean", HM = "harmonic mean", 
        GM = "geometric mean", MO = "mode", MD = "median", CL="classical parameterization",
		PO="proportional odds", QN="quantile")
    model = switch(object$dist, BE = "beta", GA = "gamma", IG = "inverse gamma", 
        BP = "beta prime", LL="log-logistic", LL2="log-logistic", LL3="log-logistic",
		LL4="log-logistic", LL5="log-logistic", LN="log-normal")
    cat("-------------------------------------------------------------------------\n")
    cat("Reparametrized", model, "regression model\n")
    cat("based on the ", tipo, " ",ifelse(object$param=="QN",object$tau,""), "\n", sep = "")
    cat("-------------------------------------------------------------------------\n")
    cat("Regression coefficients for the", tipo, "\n")
    print(tt[1:ncol(object$mu.x), , drop = FALSE])
    cat("-------------------------------------------------------------------------\n")
    cat("Regression coefficients for the precision \n")
    print(tt[ncol(object$mu.x) + 1:ncol(object$sigma.x), , drop = FALSE])
    cat("-------------------------------------------------------------------------\n")
    cat("Signif. codes:  0 \"***\" 0.001 \"**\" 0.01 \"*\" 0.05 \".\" 0.1 \" \" 1\n")
    cat("---\n")
}
