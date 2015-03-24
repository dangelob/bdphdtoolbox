#' A fonction to extract p-value from a lm object
#' 
#' @param modelobject the lm object from which we want the p-value
#' 
#' @export
# from : http://stackoverflow.com/questions/5587676/pull-out-p-values-and-r-squared-from-a-linear-regression
xtrct_pval <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}