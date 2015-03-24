#' A fonction to extract p-value from a lm object
#' 
#' @param modelobject the lm object from which we want the equation an R2
#' @param type preselection of returned elements (R2, eq, eq+R2, )
#' 
#' @export
# modified from : http://stackoverflow.com/questions/7549694/ggplot2-adding-regression-line-equation-and-r2-on-graph
lmlab_eqr2 = function(m){
  if (class(m) != "lm") stop("Not an object of class 'lm' ")
  eq <- substitute(y == a + b %.% x*","~~r^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

#' @export
lmlab_r2pval = function(m){
  if (class(m) != "lm") stop("Not an object of class 'lm' ")
  eq <- substitute(R^2~"="~r2*","~p~"<"~pval,
                   list(pval = format(0.001,digits = 3), 
                        r2 = format(summary(m)$r.squared, digits = 2)))
  return(as.character(as.expression(eq)));     
}

#' @export
lmlab_eq = function(m){
  if (class(m) != "lm") stop("Not an object of class 'lm' ")
  eq <- substitute(y == a + b %.% x, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2)))
  return(as.character(as.expression(eq)));     
}