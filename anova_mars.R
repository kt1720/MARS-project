anova.mars <- function(object = stop("mars object not provided")){
  # Find the index for non-univariate or bivariate contribution in the splits list and remove them
  index <- c(which(sapply(object$splits, nrow) != 2 & sapply(object$split, nrow) != 3)) 
  object$splits <- object$splits[-index]
  # Sort the list in an increasing order
  object$splits <- object$splits[order(sapply(object$splits, nrow))]
  
  uni <- list()
  bi <- list()
  # Basis function with 1 variable
  uni <- object$splits[(which(sapply(object$splits, nrow) == 2))]
  # Basis function with 2 variables
  bi <- object$splits[(which(sapply(object$splits, nrow) == 3))]
  
  # Create output for different cases
  if(length(uni) != 0 & length(bi) != 0){
    return(list(Univariate = uni, Bivariate = bi))
  }
  else if(length(uni) != 0 & length(bi) == 0){
    return(list(Univariate = uni))
  }
  else if(length(uni) == 0 & length(bi) != 0){
    return(list(Bivariate = bi))
  }
  else{
    cat("There are no basis functions of 1 variable or 2 variables.")
  }
}
