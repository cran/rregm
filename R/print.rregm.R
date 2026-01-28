print.rregm <-
function(x, digits = max(3L, getOption("digits") - 3L), ...) 
{
    cat("\n")
    cat("Coefficients:\n")
    print.default(format(x$estimate[,1], digits = digits), print.gap = 2L, 
        quote = FALSE)
    invisible(x)
}
