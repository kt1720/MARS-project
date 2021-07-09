#------------ MARS algorithm ------------#
mars <- function(formula, data, control = NULL, ...){
  cc <- match.call()
  mf <- model.frame(formula, data)
  y <- model.response(mf)
  mt <- attr(mf, "terms")
  x <- model.matrix(mt, mf)
  if(is.null(control)) control = mars.control()
  fwd <- fwd_stepwise(y, x, control)
  bwd <- bwd_stepwise(fwd, control)
  fit <- lm(y ~ ., data = data.frame(y = y, bwd$B[, -1]))
  structure(c(list(call = cc, formula = formula, y = y, B = bwd$B, 
                   splits = bwd$splits, data = data, varnames = colnames(x),
                   filter = bwd$filter, Mmax = bwd$Mmax, lof = bwd$lof), fit),
            class = c("mars", class(fit)))
}

fwd_stepwise <- function(y, x, control){
  N <- length(y)
  n <- ncol(x)
  B <- matrix(1, N, 1)
  splits <- list(data.frame(m=0,v=0,s=NA,t=NA))
  M <- 1
  while(M <= control$Mmax){
    if(control$trace) cat("M:", M, "\n")
    lof_best <- Inf
    for(m in 1:M){
      x_v <- setdiff(1:n, splits[[m]]$v)
      if(control$trace) cat("M:",M, "m:", m, "svars:", x_v, "\n")
      for(v in x_v){
        tt <- split_points(x[,v], B[,m])
        for(t in tt){
          Bnew <- data.frame(B[,(1:M)[-m]],
                             Btem1=B[,m]*h(x[,v], 1, t),Btem2=B[,m]*h(x[,v], -1, t))
          gdat <- data.frame(y=y, Bnew)
          lof <- LOF(y~.,gdat, control)
          if(lof < lof_best) { 
            lof_best <- lof
            m_best = m; v_best = v; t_best = t
          }
        }
      }
    }
    B <- cbind(B, B[,m_best]*h(x[,v_best], 1, t_best), B[,m_best]*h(x[,v_best], -1, t_best))
    left_split <- rbind(splits[[m_best]], c(m_best, v_best, 1, t_best))
    right_split <- rbind(splits[[m_best]], c(m_best, v_best, -1, t_best))
    splits <- c(splits, list(left_split), list(right_split))
    M <- M + 2
  }
  colnames(B) <- c("B0",paste0("B",1:(ncol(B)-1)))
  return(list(y = y, B = B, splits = splits))
}

bwd_stepwise <- function(bwd_in,control){
  Mmax = ncol(bwd_in$B)
  Jstar = 2:Mmax
  kstar = Jstar
  dat = data.frame(y=bwd_in$y,bwd_in$B)
  lofstar = LOF(y~.,dat,control)
  for(M in Mmax:2){
    b = Inf 
    L = kstar
    if(control$trace)cat("L:",L,"\n")
    for(m in L){
      K = setdiff(L,m)
      dat = data.frame(y=bwd_in$y,bwd_in$B[,K])
      lof = LOF(y~.,dat,control)
      if(control$trace)cat("M:", M,"k:",K, "lof criterion:",lof,"\n")
      if(lof < b){
        b = lof
        kstar = K
      }
      if(lof < lofstar){
        lofstar = lof
        Jstar = K
      }
    }
  }
  Jstar = c(1, Jstar)
  filter = setdiff(2:Mmax, Jstar) # Find the basis functions that got filtered out by bwd_stepwise
  filter = lapply(filter, function(x) x-1) 
  return(list(y=bwd_in$y,B=bwd_in$B[,Jstar],splits=bwd_in$splits[Jstar],filter=filter,Mmax=Mmax,lof=lofstar))
}

#------------ Supporting functions ------------#
h <- function(x,s,t){
  return(pmax(0, s*(x-t)))
}

split_points <- function(xv,Bm) {
  out <- sort(unique(xv[Bm>0]))
  return(out[-length(out)])
}

# LOF function from Friedman(1991)
LOF <- function(form, data, control){
  model <- lm(form, data)
  RSS <- sum(residuals(model)^2)
  M <- ncol(data) - 1 # -1 for y column
  N <- nrow(data)
  C_M <- sum(lm.influence(model)$hat)
  
  return(RSS*N/(N - (C_M + control$d*M))^2)
}

#------------ Constructor, validator and helper for mars.control() function ------------#
new_mars.control <- function(Mmax, d, trace){
  structure(list(Mmax = Mmax, d = d, trace = trace), class = "mars.control")
}

validate_mars.control <- function(control){
  if(control$Mmax < 2){
    warning("Mmax must be greater or equal to 2; setting to 2")
    control$Mmax = 2
  }
  control
}

mars.control <- function(Mmax = 2, d = 3, trace = FALSE){
  control <- new_mars.control(Mmax, d, trace)
  validate_mars.control(control)
}
