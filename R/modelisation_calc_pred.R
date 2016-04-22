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
  if(missing(flux)){ # If no flux argument
  dft <- df[,c(nam[3:length(nam)]), drop=FALSE]
  }else{ # If a flux argt
  dft <- df[,c(flux, nam[3:length(nam)])]
  }
  return(dft)
}

#' A fonction returning generic columns name (Y~X1+X2) (needed to calculate prediction). Usually this function is called after prd_seldat
#' 
#' @param df the dataframe on which the column name have to be changed
#' @param resp_var TRUE if the dataframe contain the response variable (Y) and FALSE otherwise
#' 
#' @export
prd_prpdat <- function(df, resp_var){
  # Check if resp_var is given
  # missing(resp_var) || stop("resp_var is missing")
  # dataframe preparation: add generic column names
  colnam <- c()
  if(resp_var){
    s <- seq(1:(NCOL(df)-1))
    colnam <- c("Y", paste0("X",s))
  }else{
    s <- seq(1:(NCOL(df)))
    colnam <- paste0("X",s)
  }
  return(colnam)
}
# prd_prpdat <- function(df){
#   # dataframe preparation: add generic column names
#   colnam <- c()
#   for(i in 1:NCOL(df)){
#     colnam <- c(colnam, paste0("X",i))
#   }
#   colnames(df) <- colnam
#   return(df)
# }

#' A fonction to calculate the predicted ER fluxes with exponential equations
#' 
#' @param df the dataframe contain the needed, AND ONLY THE NEEDED, variables (depending on the model)
#' @param p the vector containing the calibration parameter from the model
#' 
#' @export
gpER_exp <- function(df, p){
  df$Y <- NULL # not needed and messes counting as not always here
  nb_par <- length(na.omit(p))
  nb_var <- NCOL(df)
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

#' A fonction that calculate models estimation trought other function depending on equation type (linear, exponential...)
#' 
#' @param df the dataframe contain the needed, AND ONLY THE NEEDED, variables (depending on the model)
#' @param p the vector containing the calibration parameter from the model
#' @param id_eq the model identifiant look like eqtype-var1_var2 (example exp-Tair_H)
#' 
#' @export
gpER <- function(df, p, id_eq){
  # id the model
  eq_type <- prd_idfy(id_eq)[1]
  # select 
  if(eq_type == "exp"){
    pred <- gpER_exp(df, p)
  }else if(eq_type == "lin"){
    pred <- gpER_lin(df, p)
  }else if(eq_type == "pwr"){
    pred <- gpER_pwr(df, p)
  }else{
    cat("Unknown equation type")
  }
  return(pred)
}


#' A fonction to calculate the predicted ER fluxes
#' 
#' @param df the dataframe contain the needed, AND ONLY THE NEEDED, variables (depending on the model)
#' @param p the vector containing the calibration parameter from the model
#' 
#' @export
gpGPPsat <- function(df, p){
  df$Y <- NULL # not needed and messes counting as not always here
  nb_par <- length(na.omit(p))
  nb_var <- NCOL(df)
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
