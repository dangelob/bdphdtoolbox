# Commonly used equations
#' @export
mdlcalc <- function(y, x, type){
  if(type=="lin"){
    m <- lm(y~x)
  }else if(type=="exp"){
    m <- lm(log(y)~x)
  }else if(type=="arr"){
    Y <- log(y)
    X <- 1/(x+273.15)
    m <- lm(Y~X)
  }
  return(m)
}


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