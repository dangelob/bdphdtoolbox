#' A fonction to retrieve parameters calculate from calibration
#' 
#' @param id_eq the model identifiant look like eqtype-var1_var2 (example exp-Tair_H)
#' @param dfparam the dataframe containing the parameters in wide format
#' 
#' @export
prd_gpar <- function(id_eq, dfparam){
  # Retrieve parameters from calibration
  out <- dfparam %>%
    filter(mdl==id_eq)%>%
    select(-mdl)
  tp <- as.vector(t(out))
  par <- tp[!is.na(tp)]# Return as vector without NA
  return(par)
}

#' A fonction to retrieve, from the model id, the equation type, the number of variable and their names.
#' 
#' @param id_eq the model identifiant look like eqtype-var1_var2 (example exp-Tair_H)
#' 
#' @export
prd_idfy <- function(id_eq){
  # Identify model : equation type, variable number and names
  eq_type <- unlist(strsplit(id_eq, "-"))[1]
  vars <- strsplit(id_eq, "-")[[1]][2]
  var_ls <- unlist(strsplit(vars, "_"))
  out <- c(eq_type, length(var_ls), var_ls)
  return(out)
}

#' A fonction to select the data needed to calculate the predicted fluxes
#' 
#' @param df dataframe containing the variable needed to calculate the prediction
#' @param flux the flux type either "ER", "GPPsat", "GPP" and "NEE"
#' @param id_eq the model identifiant look like eqtype-var1_var2 (example exp-Tair_H)
#' 
#' @export
prd_seldat <- function(df, flux, id_eq){
  # Select data from dataframe (and check if ok ?)
  df <- as.data.frame(df) # convert df to data.frame (no "tbl_df"...)
  nam <- prd_idfy(id_eq=id_eq)
  nb_var <- as.numeric(nam[2])
  dft <- df[,c(flux, nam[3:length(nam)])]
  return(dft)
}

#' A fonction to change columns name to generic ones (needed to calculate prediction). Usually this function is called after prd_seldat
#' 
#' @param df the dataframe on which the column name have to be changed
#' 
#' @export
prd_prpdat <- function(df){
  # dataframe preparation: add generic column names
  colnam <- c("Y")
  for(i in 1:(NCOL(df)-1)){
    colnam <- c(colnam, paste0("X",i))
  }
  colnames(df) <- colnam
  return(df)
}

#' A fonction to calculate the predicted ER fluxes
#' 
#' @param df the dataframe contain the needed variables (depending on the model)
#' @param p the vector containing the calibration parameter from the model
#' 
#' @export
gpER <- function(df, p){
  nb_par <- length(na.omit(p))
  nb_var <- NCOL(df)-1
  if(nb_par == 2 && nb_var == 1){
    # a*exp(b*X1)
    pER <- p[1]*exp(p[2]*df$X1)
  }else if(nb_par == 3 && nb_var == 2){
    # (a*X2+c)*exp(b*X1)
    pER <- (p[1]*df$X2+p[3])*exp(p[2]*df$X1)
  }else{
    cat("unknown")
  }
  return(pER)
}

#' A fonction to calculate the predicted ER fluxes
#' 
#' @param df the dataframe contain the needed variables (depending on the model)
#' @param p the vector containing the calibration parameter from the model
#' 
#' @export
gpGPPsat <- function(df, p){
  nb_par <- length(na.omit(p))
  nb_var <- NCOL(df)-1
  if(nb_par == 3 && nb_var == 1){
    # a*exp(-(X1-b)/c)^2)
    pGPPsat <- p[1] * exp(-((df$X1-p[2])/p[3])^2)
  }else if(nb_par == 4 && nb_var == 2){
    # (a*X2+d)*exp(-(X1-b)/c)^2)
    pGPPsat <- (p[1]*df$X2+p[4]) * exp(-((df$X1-p[2])/p[3])^2)
  }else{
    cat("unknown")
  }
  return(pGPPsat)
}
