print.LRskov<-function (x, digits = max(3L, getOption("digits") - 3L), ...) 
{
	if(!is.null(x$statistic))
	{
		cat("Likelihood ratio test applied in the ",paste("BE",x$param,sep=""),"regression model\n")
		cat("for the coefficients related to the following\n")
		cat("covariates: ",setdiff(colnames(x$X),colnames(x$X0)),"\n")
		cat("------------------------------------------------------\n")
		cat("LR statistic  :",x$statistic[1],"p-value:", x$p.value[1],"\n")
		cat("LR1 statistic :",x$statistic[2],"p-value:", x$p.value[2],"\n")
		cat("LR2 statistic :",x$statistic[3],"p-value:", x$p.value[3],"\n")
		cat("------------------------------------------------------\n")
	}
	if(!is.null(x$MSE))
	{
		aa=cbind(round(x$MSE,4), round(x$MAE,4), round(x$LS,4))
		colnames(aa)=c("MSE","MAE","LS")
		cat("------------------------------------------------------\n")
		cat(paste("BE",x$param,sep=""),"regression model predictive shrinkage summary\n")
		cat("------------------------------------------------------\n")
		cat("Training size :",round(length(x$y)*x$train),"\n")
		cat("Test size :",round(length(x$y)*(1-x$train)),"\n")
		cat("\n")
		cat("Full model :", deparse(x$formula), "\n")
		cat("Reduced model :", deparse(x$reduced.fo), "\n")
		cat("------------------------------------------------------\n")
		cat("Out-of-sample performance\n")
		print(aa)
		cat("------------------------------------------------------\n")
		cat("Best methods:\n")
		cat("MSE      :",c("MLE","US","US1","US2","GAIC")[which.min(x$MSE)],"\n")
		cat("MAE      :",c("MLE","US","US1","US2","GAIC")[which.min(x$MAE)],"\n")
		cat("Log-Score:",c("MLE","US","US1","US2","GAIC")[which.max(x$LS)],"\n")
		cat("-------\n")
	}
   invisible(x)
}

