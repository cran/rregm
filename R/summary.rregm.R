summary.rregm <-
function (object, ...) 
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
    tt <- cbind(object$estimate[,1], object$estimate[,2], 
        object$estimate[,1]/object$estimate[,2], pnorm(abs(object$estimate[,1]/object$estimate[,2]), 
            lower.tail = FALSE))
    ast = sapply(tt[, 4], FUN = asterisk)
    tt = data.frame(round(tt, 4), ast)
    colnames(tt) <- c("coef", "s.e.", "z value", 
        "Pr(>|z|)", "")
tipo=switch(object$param,
AM="mean",HM="harmonic mean",GM="geometric mean",MO="mode",MD="median")
model=switch(object$dist,
BE="beta",GA="gamma",IG="inverse gamma",BP="beta prime")
    cat("-------------------------------------------------------------------------\n")
    cat("Reparametrized",model," regression model\n") 
    cat("based on the ", tipo,"\n", sep = "")
     cat("-------------------------------------------------------------------------\n")
     cat("Regression coefficients for the",tipo,"\n")
     print(tt[1:ncol(object$mu.x), , drop = FALSE])
     cat("---\n")
            cat("Signif. codes:  0 \"***\" 0.001 \"**\" 0.01 \"*\" 0.05 \".\" 0.1 \" \" 1\n")
            cat("-------------------------------------------------------------------------\n")
     cat("Regression coefficients for the precision \n")
            print(tt[ncol(object$mu.x)+1:ncol(object$sigma.x), , drop = FALSE])
    cat("---\n")
}
