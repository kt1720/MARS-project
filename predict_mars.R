predict.mars <- function(object = stop("mars object not provided"), newdata = NULL){
  if(missing(newdata) || is.null(newdata)){         
    pred <- object$fitted.values
  }
  else{
    tt <- terms(object$formula)
    tt <- delete.response(tt)
    mf <- model.frame(tt, newdata)
    mt <- attr(mf, "terms")
    X <- model.matrix(mt, mf)      # Set up the newdata matrix in the proper format
    B <- matrix(1, nrow(X), ncol=length(object$splits))   
    for(i in 1:length(object$splits)){    # Loop through the splits list to create the basis matrix
      if(length(object$splits[[i]]$v) > 1){
        for(v in 2:length(object$splits[[i]]$v)){ # Loop through the child splits to find the correct basis
          B[, i] = B[, i] * h(X[, object$splits[[i]]$v[v]], object$splits[[i]]$s[v],
                              object$splits[[i]]$t[v])
        }
      }  
      else{
        B[, i] = X[, i]
      }
    }
    pred = drop(B %*% object$coefficients)
  }
  return(pred)
}
