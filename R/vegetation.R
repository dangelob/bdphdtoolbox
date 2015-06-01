#' A function to calculate the surface area in mm2 of molinia leaf
#' 
#' @param L leaf length in mm
#' 
#' @export
mol_LtoS <- function(L){
  S <- ((2.2971*L) - 36.5926)/.66
  return(S)
}