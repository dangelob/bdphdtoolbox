#' A fonction to plot measured and modelled data
#' 
#' @param Y measured data
#' @param pY modelled data
#' @param x_lim X axis limits (2 value vector)
#' @param y_lim Y axis limits (2 value vector)
#' @param mod_label custom label
#' @param mes_label custom label
#' 
#' @export
plt_mesmod <- function(Y, pY, x_lim=c(0,15), y_lim=c(0,15), mod_label="modelled", mes_label="measured"){
  par(mar=c(4,4.5,.5,.5))
  plot(pY~Y, 
       ylim=y_lim, xlim=x_lim,
       xlab="", ylab="")
  title(ylab=bquote(paste(.(mod_label)," (", mu, mol,m^-2,s^-1,")", sep="")), line=2.5)
  title(xlab=bquote(paste(.(mes_label)," (", mu, mol,m^-2,s^-1,")", sep="")), line=2 )
  abline(a=0, b=1, col="black", lty=2)
  text(19,20, "1:1", srt=45)
}


#' A fonction to plot residuals and modelled data
#' 
#' @param Y measured data
#' @param pY modelled data
#' @param x_lim X axis limits (2 value vector)
#' @param mod_label custom label
#' @param mes_label custom label
#' 
#' @export
plt_resmod <- function(Y, pY, x_lim=c(0,15), mod_label="predicted values", res_label="residuals"){
  par(mar=c(4,4,.5,.5))
  plot((Y-pY)~pY,
       xlim=x_lim,
       xlab="", ylab="")
  title(xlab=mod_label, line=2.5)
  title(ylab=res_label, line=2)
}