exr_base <- function(Y, Tr, X,
                     family = "gaussian",
                     cate_method = "X-learner",
                     cate_external = NULL,
                     uncertainty = FALSE, ci = 0.95,
                     sate_estimate = NULL,
                     boot = FALSE, nboot = 10,
                     const_list = NULL,
                     lib = c("mean", "glm", "ranger"),
                     numCores = 1,
                     clusters = NULL,
                     threshold = 0, sign = 1, loss = "raking", cut = -0.01,
                     verbose = FALSE, tolerance = 1e-05, solver = "SCS",
                     seed = 1234,
                     dual = FALSE){

  # setup
  n <- length(Y)

  if(is.null(clusters)){
    boot_ind <- seq(1:n)
  }else{
    boot_ind <- clusters
  }

  if(is.null(cate_external) == TRUE){
    cate_number <- length(cate_method)
    cate_method_name <- cate_method
  }else{
    cate_number <- ncol(cate_external)
    cate_method_name <- colnames(cate_external)
  }

  # setup
  est <- est_unc <- c()
  tau_m <- w_m <- w_unc_m <- matrix(NA, nrow = n, ncol = cate_number)
  w_m_combined <- w_unc_m_combined <- matrix(NA, nrow = n, ncol = cate_number)
  w_m_orig <- matrix(NA, nrow = n, ncol = cate_number)
  tpate_boot <- matrix(NA, nrow = nboot, ncol = cate_number)
  se <- c()
  detail <- lm_vi <- list()

  for(z in 1:cate_number){

    cate_method_use <- cate_method_name[z]

    if(is.null(cate_external) == TRUE){
      if(boot == FALSE | uncertainty == FALSE){
        cat(paste0("Estimating CATE with ", cate_method_use, "..."))
      }
      # fit the model and predict tau using the entire data
      fit_tau <- tau_fit(Y = Y, Tr = Tr, X = X, family = family, new_X = X,
                         cate_method = cate_method_use, lib = lib, seed = seed)
      tau_m[,z] <- tau <- fit_tau$test_tau
    }else{
      tau_m[,z] <- tau <- cate_external[, z]
    }

    # 1. point-estimate
    if(boot == FALSE | uncertainty == FALSE){
      cat("\nEstimating External Robustness...\n")
    }

    seed_use <- seed
    if(dual == TRUE){
      w_out <- exr_W_dual(score = tau,
                          const_list = const_list, X = X,
                          threshold = threshold, sign = sign, loss = loss, cut = cut,
                          verbose = verbose, tolerance = tolerance,
                          solver = solver, seed = seed)
      seed_use <- w_out$seed_use
    }else{
      suppressWarnings(w_out <- exr_W(score = tau,
                                      const_list = const_list, X = X,
                                      threshold = threshold, sign = sign, loss = loss, cut = cut,
                                      verbose = verbose, tolerance = tolerance,
                                      solver = solver))
    }


    est[z] <- w_summary(w_out$est_w, loss = loss)

    if(is.null(w_out$est_w) == FALSE){
      w_m[, z] <- w_out$est_w
    }
    if(is.null(w_out$est_w_combined) == FALSE){
      w_m_combined[, z] <- w_out$est_w_combined
    }
    if(is.null(w_out$orig_w) == FALSE){
      w_m_orig[, z] <- w_out$orig_w
    }

    if(is.null(w_out$est_combined) == FALSE){
      initial_w <- w_out$est_combined # we use est_combined here
    }else{
      initial_w <- rep(1, n)
    }

    if(uncertainty == TRUE){

      if(boot == FALSE){
        # fast approximation by the SATE standard error
        se[z] <- sate_estimate[2]
      }else if(boot == TRUE){
        # 2. With inference
        cat(paste0("Bootstrap (", nboot, "): Estimating CATE with ", cate_method_use, "...\n"))
        if(numCores == 1){
          oplan <- plan(sequential)
        }else{
          # plan(multisession, workers = numCores)
          # plan(multiprocess(workers = numCores))
          oplan <- plan(multisession, workers = numCores)
          on.exit(plan(oplan), add = TRUE)
        }
        tpate_boot_out <- tpate_boot(x = 1:nboot,
                                     w = initial_w,
                                     Y = Y, Tr = Tr, X = X, family = family,
                                     cate_method_use = cate_method_use, lib = lib,
                                     n = n, boot_ind = boot_ind, seed = seed)

        tpate_boot[,z] <- unlist(tpate_boot_out)
        se[z]  <- sd(tpate_boot[,z], na.rm = TRUE) # remove NA

        if(is.numeric(se[z]) == FALSE){
          warning(" Bootstrap fails and the SATE standard error is used as approximation. ")
          se[z] <- sate_estimate[2]
        }
      }


      # Estimate External Robustness after taking into account uncertainty
      quant <- qnorm(1 - (1-ci)/2)
      if(sign == 1){threshold_unc <- quant*se[z]}
      if(sign == -1){threshold_unc <- -quant*se[z]}


      if(boot == TRUE & uncertainty == TRUE){
        cat("\nEstimating External Robustness...\n")
      }
      if(dual == TRUE){
        w_unc_out <- exr_W_dual(score = tau,
                                const_list = const_list, X = X,
                                threshold = as.numeric(threshold_unc),
                                sign = sign, loss = loss,
                                cut = cut,
                                verbose = verbose,
                                tolerance = tolerance,
                                solver = solver,
                                seed = seed)
      }else{
        suppressWarnings(w_unc_out <- exr_W(score = tau,
                                            const_list = const_list, X = X,
                                            threshold = as.numeric(threshold_unc),
                                            sign = sign, loss = loss,
                                            cut = cut,
                                            verbose = verbose,
                                            tolerance = tolerance,
                                            solver = solver))
      }


      est_unc[z] <- w_summary(w_unc_out$est_w, loss = loss)

      if(is.null(w_unc_out$est_w) == FALSE){
        w_unc_m[, z] <- w_unc_out$est_w
      }
      if(is.null(w_unc_out$est_w_combined) == FALSE){
        w_unc_m_combined[, z] <- w_unc_out$est_w_combined
      }

      # if(w_unc_out$feasible == FALSE){
      #   w_unc_m[, z] <- rep(NA, n)
      # }else{
      #   w_unc_m[, z] <- w_unc_out$est_w
      #   w_unc_m_combined[, z] <- w_unc_out$est_w_combined
      # }

    }

    # # 3. linear projection on X after scaling
    # X_t  <- X[, (apply(X, 2, var) > 0), drop = FALSE]
    # var_d <- data.frame(tau, scale(X_t))
    # colnames(var_d) <- c("Y", colnames(X_t))
    # suppressWarnings({lm_vi[[z]] <- lm(Y ~ ., data = var_d)})

    # if(cate_method_use %in% c("X-learner", "T-learner", "S-learner")){
    #   detail[[z]] <- fit_tau$detail
    # }else if(cate_method_use == "lm"){
    #   detail[[z]] <- fit_tau$detail
    # }else{
    #   detail[[z]] <- NA
    # }

  }

  out <- list("est" = est, "est_unc" = est_unc,
              "w" = w_m, "w_unc" = w_unc_m,
              "w_combined" = w_m_combined, "w_unc_combined" = w_unc_m_combined,
              "w_first" = w_m_orig,
              "tau" = tau_m,
              "tpate_boot" = tpate_boot,
              "se" = se,
              "lm_vi" = lm_vi,
              "detail" = detail)
  return(out)
}
