plot.mars <- function(object = stop("mars object not provided"), x_var = NULL, color = "dark blue"){

if(is.null(x_var)){
  # set to first explanatory variable in formula
  tt <- all.vars(object$formula)
  x_var <- tt[2]
}
  
# Check that x_var is given as a string
if(!is.character(x_var)) stop("x_var name not provided as character")

  # prepare for non-numeric data
 if(!is.numeric(object$data[,x_var])){
  ff <- data.frame(group = as.factor(object$data[,x_var]), res = object$residuals)
  boxplot(data = ff, res~group, 
          horizontal = FALSE,
          names = levels(ff[,1]),
          main = paste0("Residual Boxplots for ", x_var, " Classification"),
          ylab = "Residuals",
          xlab = paste0(x_var),
          col = color)
      return(invisible()) 
}
split <- c()
base_sum <- rep(0, times = length(object$data[, x_var]))
for(i in seq(object$splits)){
  if(nrow(object$splits[[i]]) == 2){
    if(object$splits[[i]]$v[[2]] == match(x_var, all.vars(object$formula))){
      base_sum <- base_sum + 
        object$coefficients[[i]]*h(object$data[,x_var], object$splits[[i]]$s[2],
                                 object$splits[[i]]$t[2])
      split <- c(split, object$splits[[i]]$t[2])
    }
  }
}
plot(x = object$data[, x_var],
     y = base_sum,
     xlab = paste0(x_var),
     ylab = paste0("f(", x_var, "), contribution to ", all.vars(object$formula)[1]),
     main = "Univariate ANOVA Decomposition",
     type = "p",
     col = color)
abline(v = split, col = "red")
axis(1, at=split,labels=split, col = "red", col.axis = "red")
}
