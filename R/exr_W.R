exr_W <- function(score,
                  const_list = NULL, X = NULL,
                  threshold = 0, sign = 1, loss = "raking", cut = -0.01,
                  verbose = FALSE, tolerance = 1e-05, solver = "SCS"){

  # check feasibility
  feasible <- TRUE
  if(sign == 1){
    if(all(as.numeric(score) > threshold) == TRUE){
      orig_w <- est_w_combined <- est_w <- NULL
      feasible <- FALSE
    }
  }else if(sign == -1){
    if(all(as.numeric(score) < threshold) == TRUE){
      orig_w <- est_w_combined <- est_w <- NULL
      feasible <- FALSE
    }
  }
  if(feasible == TRUE){
    # setup
    ss <- length(score)

    # whether you first estimate weights
    if(is.null(const_list) == TRUE){
      orig_w <- rep(1, ss)
    }else{
      w_const <- estimate_W_const(const_list = const_list,
                                  X = X,
                                  loss = loss,
                                  cut = cut, verbose = verbose, tolerance = tolerance,
                                  solver = solver)
      if(w_const$feasible == FALSE){
        stop(" `const_list` is infeasible. Please check `const_list.` ")
      }else{
        orig_w <- w_const$est_w
      }
    }

    w <- Variable(ss)
    w_use <- orig_w * w
    # const1 <- sum(w * orig_w) == ss
    const1 <- w >= 0

    if(sign == 0){
      const2_1 <- sum(score * w * orig_w) >= sum(w * orig_w)*(threshold + tolerance)  # the same as raking
      const2_2 <- sum(score * w * orig_w) <= sum(w * orig_w)*(threshold + tolerance) # the same as raking
      constraints <- list(const1, const2_1, const2_2)
    }else if(sign == 1){
      const2 <- sum(score * w * orig_w) <= sum(w * orig_w)*threshold
      constraints <- list(const1, const2)
    }else if(sign == -1){
      const2 <- sum(score * w * orig_w) >= sum(w * orig_w)*threshold
      constraints <- list(const1, const2)
    }
    if(is.null(const_list) == FALSE){
      const_use_f <- NULL
      for(i in 1:length(const_list)){
        vars <- const_list[[i]]$vars
        values <- const_list[[i]]$values
        if(const_list[[i]]$type == "between"){
          const_use_f <- append(const_use_f, eval(const_list[[i]]$constraint[[1]]))
          const_use_f <- append(const_use_f, eval(const_list[[i]]$constraint[[2]]))
        }else{
          const_use_f <- append(const_use_f, eval(const_list[[i]]$constraint))
        }
      }
      constraints <- append(constraints, const_use_f)
    }

    # constraints <- append(constraints, list(sum(w_use) == ss)) # each step
    # constraints <- append(constraints, list(sum(w) == ss))
    # constraints <- append(constraints, list(sum(w) == 1))

    constraints <- append(constraints, list(sum(w_use) == 1)) # try with sum w_1w_2 = 1

    ## Raking (the same as "survey" package)
    if(loss == "raking"){
      Phi_R <- Minimize(sum(-entr(w_use) - w_use + 1))  # raking
    }else if(loss == "KL"){
      Phi_R <- Minimize(sum(-entr(w_use)))  # KL
    }else if(loss == "LS"){
      Phi_R <- Minimize(sum((w_use - 1)^2))  # LS
    }

    p <- Problem(Phi_R, constraints)
    suppressWarnings(res <- solve(p, verbose = verbose, solver = solver))

    if(res$status == "solver_error"){
      stop(" Optimization fails. ")
      est_w_combined <- est_w <- NULL
    }else{
      suppressWarnings(w_sol <- res$getValue(w))

      est_w0_combined <- orig_w * w_sol
      est_w_combined  <- est_w0_combined/mean(na.omit(est_w0_combined))
      est_w_combined  <- trimming(est_w_combined, cut = cut)

      est_w0 <- w_sol
      est_w  <- est_w0/mean(na.omit(est_w0))
      est_w  <- trimming(est_w, cut = cut)
    }
  }
  if(is.null(est_w)){
    feasible <- FALSE
  }
  return(list("est_w" = est_w, "est_w_combined" = est_w_combined, "orig_w" = orig_w, "feasible" = feasible))
}
