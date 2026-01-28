res <-
function (object, type="pearson") 
{
    if (!inherits(object,"rregm")) stop("class for object is not rregm")
    if (!any(type == c("pearson", "mod.pearson", "quantile"))) stop("type of residual is not avaliable")
res=as.vector(switch(type, pearson=object$pearson.res,
mod.pearson=object$mod.pearson.res,
quantile=object$quant.res))
 if(is.null(res)) stop("This type of residuals is not defined for this data set\n Try with mod.pearson or quantile options")
res
}
