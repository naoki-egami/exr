estimate_W_const_dual <- function(const_list, X,
                                  loss = "raking",
                                  cut = - 0.01, verbose = FALSE, tolerance = 1e-05,
                                  solver = "ECOS"){


  # Future: check feasibility #
  feasible <- TRUE
  ss <- nrow(X)

  if(feasible == TRUE){
    # setup
    const_mat <- matrix(NA, nrow = 0, ncol = nrow(X))
    b_vec <- c()
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
    # Phi_R <- Maximize(- t(b_vec)%*%lambda -log_sum_exp(-t(const_mat)%*%lambda))  # LS


    # Phi_R <- Maximize(- t(b_vec)%*%lambda - log_sum_exp(-t(const_mat)%*%lambda))
    const_mat_adj <- const_mat - b_vec
    Phi_R <- Maximize(-sum(exp(-t(const_mat_adj)%*%lambda)))

    # Taylor
    # const_mat_avg <- as.vector(apply(const_mat, 1, sum))
    # f2 <- (const_mat_avg %*% t(const_mat_avg))/ss^2 - (const_mat %*% t(const_mat))/ss
    # f1 <- - t(b_vec) + const_mat_avg/ss
    # Phi_R <- Maximize(f1%*%lambda + quad_form(lambda, f2))
    # Phi_R <- Maximize(f1%*%lambda + t(lambda)%*%f2%*%lambda)
    p <- Problem(Phi_R, constraints)
    suppressWarnings(res <- solve(p, verbose = verbose, solver = solver))

    if(res$status == "solver_error"){
      # re-run the code with different solver.
      suppressWarnings(res <- solve(p, verbose = verbose, solver = "SCS"))
      if(res$status != "optimal"){
        feasible <- FALSE
        est_w <- NULL
      }
    }
    if(feasible == TRUE){

      lambda_sol <- res$getValue(lambda)
      nu_sol <- log(sum(exp(-t(const_mat)%*%lambda_sol))) - 1
      w_sol <- ss/exp(t(const_mat)%*%lambda_sol + nu_sol + 1)

      est_w0 <- w_sol
      est_w  <- est_w0/mean(na.omit(est_w0))
      est_w <- trimming(est_w, cut = cut)
    }
    if(is.null(est_w)){
      feasible <- FALSE
    }
  }
  return(list("est_w" = est_w, "feasible" = feasible))
}
