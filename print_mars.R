print.mars <- function(object = stop("mars object not provided")){
  cat("Call:\n", paste(deparse(object$call), sep="\n", collapse = "\n"), "\n", sep = "")
  if(length(object$coefficients) != 0){
    cat("\nCoefficients:\n")
    cn <- names(object$coefficients)
    coefs <- matrix(object$coefficients, length(cn), 1, dimnames = list(cn))
    colnames(coefs) <- "Estimates"
    printCoefmat(t(coefs))
    cat("\nSelected", ncol(object$B)-1, "of", object$Mmax-1, "basis functions\n\n", sep = " ")
  }
  else{
    cat("No coefficients\n\n")
  }
}