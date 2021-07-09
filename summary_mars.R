summary.mars <- function(object = stop("No 'object' argument")){
  # Include function call
  cat("Call:\n", paste(deparse(object$call), sep="\n", collapse = "\n"), "\n", sep = "")
  # Include coefficients for each basis functions
  if(ncol(object$B) == 0){
    cat("There are no basis functions in this function!\n")
  }
  else{
    cat("\nCoefficients:\n")
    cn <- names(object$coefficients)
    coefs <- matrix(object$coefficients, length(cn), 1, dimnames = list(cn))
    colnames(coefs) <- "Estimates"
    printCoefmat(t(coefs))
    # Include basic information about the basis functions
    cat("\nBasis function(s):")
    Bs <- colnames(object$B)
    for(i in 2:length(Bs)){
      cat("\n", Bs[i], ":\n    ", sep = "")
      for(f in 2:length(object$splits[[i]]$v)){
        cat("Component ", f-1, ": variable = ", object$varnames[object$splits[[i]]$v[f]], 
            "; sign = ", object$splits[[i]]$s[f], "; split at value: ", object$splits[[i]]$t[f],  
            "; hinge: h(", ifelse(object$splits[[i]]$s[f] == 1, 
            paste0(object$varnames[object$splits[[i]]$v[f]], "-", object$splits[[i]]$t[f], sep = ""),
            paste0(object$splits[[i]]$t[f], "-", object$varnames[object$splits[[i]]$v[f]], sep = "")),
            ")\n    ", sep = "")
      }
    }
    # Summary of the fitting process
    cat("\nBasis function(s) with interaction: ")
    index <- which(sapply(object$splits, nrow) > 2)
    if(length(index) > 0){
      for(l in index[-length(index)]){
        cat(paste0(colnames(object$B)[l], sep = ", "))
      }
      cat(colnames(object$B)[index[length(index)]])
    }
    else{
      cat("None")
    }
    cat("\nSelected", length(Bs)-1, "of", object$Mmax-1, "basis functions", sep = " ")
    cat("\nBasis function(s) filtered out by backward stepwise: ")
    for(k in object$filter[-length(object$filter)]){
      cat(paste0("B", k, sep = ", "))
    }
    cat(paste0("B", object$filter[length(object$filter)],
               "\nGCV criterion: ", round(object$lof, 2),
               "\tRSS: ", round(sum(residuals(object)^2), 2), "\n\n", sep = ""))
  }
}  