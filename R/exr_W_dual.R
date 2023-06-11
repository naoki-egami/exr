exr_W_dual <- function(score,
                       const_list = NULL, X = NULL,
                       threshold = 0, sign = 1, loss = "raking", cut = -0.01,
                       verbose = FALSE, tolerance = 1e-05, solver = "ECOS", seed = 1234){

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
      w_const <- estimate_W_const_dual(const_list = const_list,
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

    # w <- Variable(ss)
    # w_use <- orig_w * w
    # # const1 <- sum(w * orig_w) == ss
    # const1 <- w >= 0
    #
    # if(sign == 0){
    #   const2_1 <- sum(score * w * orig_w) >= sum(w * orig_w)*(threshold + tolerance)  # the same as raking
    #   const2_2 <- sum(score * w * orig_w) <= sum(w * orig_w)*(threshold + tolerance) # the same as raking
    #   constraints <- list(const1, const2_1, const2_2)
    # }else if(sign == 1){
    #   const2 <- sum(score * w * orig_w) <= sum(w * orig_w)*threshold
    #   constraints <- list(const1, const2)
    # }else if(sign == -1){
    #   const2 <- sum(score * w * orig_w) >= sum(w * orig_w)*threshold
    #   constraints <- list(const1, const2)
    # }
    #
    # constraints <- append(constraints, list(sum(w_use) == ss)) # each step
    # constraints <- append(constraints, list(sum(w) == ss))

    # ################################
    # New Version with Dual Form
    # ################################
    const_mat <- matrix(NA, nrow = 1, ncol = length(score))
    if(sign == 1){
      b_vec <- threshold
      const_mat[1,] <- score
    }else if(sign == -1){
      b_vec <- - threshold
      const_mat[1,] <- - score  # row = number of constraints, column = number of units
    }
    if(is.null(const_list) == FALSE){
      for(i in 1:length(const_list)){
        vars <- const_list[[i]]$vars
        values <- const_list[[i]]$values
        if(const_list[[i]]$type == "between"){
          const_mat <- rbind(const_mat, rbind(-X[, vars], X[, vars]))
          b_vec <- c(b_vec, -values[1], values[2])
        }else if(const_list[[i]]$type == "less than or equal to"){
          const_mat <- rbind(const_mat, X[, vars])
          b_vec <- c(b_vec, values[1])
        }else if(const_list[[i]]$type == "larger than or equal to"){
          const_mat <- rbind(const_mat, -X[, vars])
          b_vec <- c(b_vec, -values[1])
        }
      }
    }

    lambda <- Variable(nrow(const_mat))
    constraints <- list(lambda >= 0)

    ## Raking (the same as "survey" package)
    # Phi_R <- Maximize(-b*lambda -log(sum(exp(-lambda*score))))  # LS
    # Phi_R <- Maximize(- t(b_vec)%*%lambda - log_sum_exp(-t(const_mat)%*%lambda))
    const_mat_adj <- const_mat - b_vec
    Phi_R <- Maximize(- sum(exp(-t(const_mat_adj)%*%lambda)))
    # Taylor
    # const_mat_avg <- as.vector(apply(const_mat, 1, sum))
    # f2 <- (const_mat_avg %*% t(const_mat_avg))/ss^2 - (const_mat %*% t(const_mat))/ss
    # f1 <- - t(b_vec) + const_mat_avg/ss
    # Phi_R <- Maximize(f1%*%lambda + quad_form(lambda, f2))
    # Phi_R <- Maximize(f1%*%lambda + t(lambda)%*%f2%*%lambda)

    seed_use <- seed
    set.seed(seed_use)
    p <- Problem(Phi_R, constraints)
    suppressWarnings(res <- solve(p, verbose = verbose, solver = solver))

    if(res$status == "solver_error"){
      # re-run the code with different solver.
      suppressWarnings(res <- solve(p, verbose = verbose, solver = "SCS"))

      # Try with 5 different seeds
      if(res$status != "optimal"){
        for(i in 1:5){
          seed_use <- seed + i
          set.seed(seed_use)
          suppressWarnings(res <- solve(p, verbose = verbose, solver = "SCS"))
          if((res$status == "optimal" | res$status == "optimal_inaccurate")) break
        }
      }
      if((res$status == "optimal" | res$status == "optimal_inaccurate") == FALSE){
        feasible <- FALSE
        est_w_combined <- est_w <- NULL
        stop(" Optimization fails. Please check data structure. ")
      }
    }
    if(feasible == TRUE){
      lambda_sol <- res$getValue(lambda)
      nu_sol <- log(sum(exp(-t(const_mat)%*%lambda_sol))) - 1
      w_sol <- ss/exp(t(const_mat)%*%lambda_sol + nu_sol + 1)

      est_w0_combined <- w_sol
      est_w_combined  <- est_w0_combined/mean(na.omit(est_w0_combined))
      est_w_combined  <- trimming(est_w_combined, cut = cut)

      est_w0 <- w_sol/orig_w
      est_w  <- est_w0/mean(na.omit(est_w0))
      est_w <- trimming(est_w, cut = cut)
    }
  }
  if(is.null(est_w)){
    feasible <- FALSE
  }
  return(list("est_w" = est_w, "est_w_combined" = est_w_combined, "orig_w" = orig_w, "feasible" = feasible, "seed" = seed_use))
}
