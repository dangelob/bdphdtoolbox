#' A fonction to transcode p-value to significant star
#' 
#' @param vec the vector to transcode
#' 
#' @export

tc_pval <- function(vec){
  # Passer la colonne pvalue en factor
  vec <- cut(vec, breaks=(c(min(vec), 0.001, 0.01, 0.05, (max(vec)+1))), right=FALSE)
  # Changer nom factor
  levels(vec) <- c("***", "**", "*" ,"-")
  return(vec)
}