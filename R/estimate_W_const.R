estimate_W_const <- function(const_list, X,
                             loss = "raking",
                             cut = - 0.01, verbose = FALSE, tolerance = 1e-05,
                             solver = "SCS"){


  # Future: check feasibility #
  feasible <- TRUE
  ss <- nrow(X)

  if(feasible == TRUE){
    # setup
    w <- Variable(ss)
    orig_w <- rep(1, ss)

    const_use_f <- NULL
    for(i in 1:length(const_list)){
      vars <- const_list[[i]]$vars
      values <- const_list[[i]]$values
      if(vars %in% colnames(X)){
        if(const_list[[i]]$type == "between"){
          const_use_f <- append(const_use_f, eval(const_list[[i]]$constraint[[1]]))
          const_use_f <- append(const_use_f, eval(const_list[[i]]$constraint[[2]]))
        }else{
          const_use_f <- append(const_use_f, eval(const_list[[i]]$constraint))
        }
      }
    }
    constraints <- append(const_use_f, list(w >= 0))
    # constraints <- append(const_use_f, list(sum(w) == ss))
    constraints <- append(const_use_f, list(sum(w) == 1))

    ## Raking (the same as "survey" package)

    if(loss == "raking"){
      Phi_R <- Minimize(sum((-entr(w) - w + 1)))  # raking
    }else if(loss == "KL"){
      Phi_R <- Minimize(sum((-entr(w))))  # KL
    }else if(loss == "LS"){
      Phi_R <- Minimize(sum((w - 1)^2))  # LS
    }
    p <- Problem(Phi_R, constraints)
    suppressWarnings(res <- solve(p, verbose = verbose, solver = solver))

    if(res$status != "optimal"){
      est_w <- NULL
      feasible <- FALSE
    }else{
      est_w0 <- orig_w * res$getValue(w)
      est_w  <- est_w0/mean(na.omit(est_w0))
      est_w <- trimming(est_w, cut = cut)
      feasible <- TRUE
    }
    if(is.null(est_w)){
      feasible <- FALSE
    }
  }
  return(list("est_w" = est_w, "feasible" = feasible))
}
