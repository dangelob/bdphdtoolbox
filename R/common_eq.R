# Individual model equations

#' A function to calculate linear model from 2 vectors
#' 
#' @param y the variable to explain
#' @param x the predictor
#' 
#' @export
lmc_lin <- function(y, x){
  m <- lm(y~x)
  return(m)
}
#' A function to calculate exponential model from 2 vectors
#' 
#' @param y the variable to explain
#' @param x the predictor
#' 
#' @export
lmc_exp <- function(y, x){
  m <- lm(log(y)~x)
  return(m)
}
#' A function to calculate power model from 2 vectors
#' 
#' @param y the variable to explain
#' @param x the predictor
#' 
#' @export
lmc_pwr <- function(y, x){
  m <- lm(log(y)~log(x))
  return(m)
}
#' A function to calculate arrhenius model from 2 vectors
#' 
#' @param y the variable to explain
#' @param x the predictor
#' 
#' @export
lmc_arr <- function(y, x){
  Y <- log(y)
  X <- 1/(x+273.15)
  m <- lm(Y~X)
  return(m)
}

# Selection function

#' A function to calculate either linear, exponential or arrhenius model from 2 vectors
#' 
#' @param y the variable to explain
#' @param x the predictor
#' @param type "lin", "exp" or "arr" for linear, exponential and arrhenius equations
#' 
#' @export
lmc_sel <- function(y, x, type){
  if(type=="lin"){
    m <- lmc_lin(y, x)
  }else if(type=="exp"){
    m <- lmc_exp(y, x)
  }else if(type=="pwr"){
    m <- lmc_pwr(y, x)
  }else if(type=="arr"){
    m <- lmc_arr(y, x)
  }
  return(m)
}

#' A function to calculate linear, exponential or arrhenius model from 2 vectors and returning a dataframe with parameters
#' 
#' @param y the variable to explain
#' @param x the predictor
#' @param type "lin", "exp" or "arr" for linear, exponential and arrhenius equations
#' 
#' @export
lmc_calc_all <- function(y, x, details="all"){
  #linear
  lin <- lm_get_std(lmc_lin(y = y, x = x), details=details)
  lin$equation <- "linear"
  lin$a <- lin$intercept
  lin$b <- lin$slope
  #exponential
  exp <- lm_get_std(lmc_exp(y = y, x = x), details=details)
  exp$equation <- "exponential"
  exp$a <- exp(exp$intercept)
  exp$b <- exp$slope
  #power
  pwr <- lm_get_std(lmc_pwr(y = y, x = x), details=details)
  pwr$equation <- "power"
  pwr$a <- exp(pwr$intercept)
  pwr$b <- pwr$slope
  #arrhenius
  arr <- lm_get_std(lmc_arr(y = y, x = x), details=details)
  arr$equation <- "arrhenius"
  arr$a <- exp(exp$intercept)
  arr$b <- -arr$slope*8.314472
  #return all 
  result <- rbind(rbind(lin, exp), arr)
  return(result)
}

#' A function retrieve residuals from different equations






#' A fonction extract common parameter from a linear model
#' 
#' @param modelobj the model from which we want the parameters.
#' @param details "min", "extended" or "all" to choose pre-selected set of parameters to be returned
#' 
#' @export
lm_get_std <- function(modelobj, details="min"){
  if (class(modelobj) != "lm") stop("Not an object of class 'lm' ")
  m <- summary(modelobj)
  # min
  intercept <- unname(coef(modelobj)[1])
  slope <- unname(coef(modelobj)[2])
  R <- cor(modelobj$model)[1,2]
  r2 <- summary(modelobj)$r.squared
  r2a <- summary(modelobj)$adj.r.squared
  p <- xtrct_pval(modelobj)
  # extended
  rse <- m$sigma
  AIC <- AIC(modelobj)
  BIC <- BIC(modelobj)
  # all
  intercept_se <- coef(m)[3]
  slope_se <- coef(m)[4]
  intercept_p <- coef(m)[7]
  slope_p <- coef(m)[8]
  intercept_t <- coef(m)[5]
  slope_t <- coef(m)[6]
  # parameters selection
  if(details=="min"){
  result <- data.frame(intercept=intercept, slope=slope, R=R, R2=r2, aR2=r2a, pval=p)
  }else if(details=="extended"){
  result <- data.frame(intercept=intercept, slope=slope, R=R, R2=r2, aR2=r2a, pval=p, RSE=rse, AIC=AIC, BIC=BIC)  
  }else if(details=="all"){
  result <- data.frame(intercept=intercept, slope=slope, R=R, R2=r2, aR2=r2a, pval=p, RSE=rse, AIC=AIC, BIC=BIC, intercept_se=intercept_se, slope_se=slope_se, intercept_p=intercept_p, slope_p=slope_p, intercept_t=intercept_t, slope_t=slope_t)  
  }else{
    stop("details should be one of the following : min, extended or all")
  }
  return(result)
}

#' A fonction extract common parameter from an exponential model
#' 
#' @param modelobj the model from which we want the parameters.
#' 
#' @export
lm_get_exp <- function(modelobj, details="min"){
  if (class(modelobj) != "lm") stop("Not an object of class 'lm' ")
  std <- lm_get_std(modelobj, details)
  std$q10 <- exp(10*std$slope)
  return(std)
}

#' A fonction extract common parameter from an arrhenius model: WARNING to do
#' 
#' @param modelobj the model from which we want the parameters.
#' 
#'
lm_get_arr <- function(modelobj){
  if (class(modelobj) != "lm") stop("Not an object of class 'lm' ")
  std <- lm_get_std(modelobj)
  std$Ea <- std$slope/1000 # Ea en kJ
  return(std)
}


# lm_get_lin <- function(modelobj){
#   if (class(modelobj) != "lm") stop("Not an object of class 'lm' ")
#   R <- co r(modelobj$model)[1,2]
#   r2 <- summary(modelobj)$r.squared
#   intercept <- unname(coef(modelobj)[1])
#   slope <- unname(coef(modelobj)[2])
#   result <- data.frame(R2=r2, intercept=intercept, slope=slope, R=R)
#   return(result)
# }