#' Summary function
#' @param object the output from exr()
#' @param digits digits used for a table of covariate profiles. Default = 2.
#' @param ... Other arguments.
#' @export

summary.exr <- function(object, digits = 2, ...){

  # #################
  # Housekeeping
  # #################
  # data(bench_prob_survey)
  exr_nrs <- 0.1437875
  # data(bench_mturk)
  exr_Mturk <- 0.5659691

  if(object$input$uncertainty == TRUE){
    est_exr <- data.frame(cbind(object$est, object$est_unc))
    colnames(est_exr) <- c("Estimate", "With Uncertainty")
    est_exr$Method <- object$input$cate_method
  }else{
    est_exr <- data.frame(cbind(object$est))
    colnames(est_exr) <- c("Estimate")
    est_exr$Method <- object$input$cate_method
  }

  # add significance
  sig <- rep("", length(est_exr[, "Estimate"]))
  sig[est_exr[ , "Estimate"]  > exr_Mturk] <- "**"
  sig[est_exr[ , "Estimate"]  >= exr_nrs & est_exr[ , "Estimate"]  < exr_Mturk] <- "*"
  est_exr$Sig <- sig
  # col_name <- c("Method", "space", "Estimate", "Sig")
  col_name <- c("space", "Estimate", "Sig")

  if(object$input$uncertainty == TRUE){
    sig_unc <- rep("", length(est_exr[, "With Uncertainty"]))
    sig_unc[est_exr[ , "With Uncertainty"]  > exr_Mturk] <- "**"
    sig_unc[est_exr[ , "With Uncertainty"]  >= exr_nrs & est_exr[ , "With Uncertainty"]  < exr_Mturk] <- "*"
    est_exr$Sig_unc <- sig_unc
    # col_name <- c("Method", "space", "Estimate", "Sig", "With Uncertainty", "Sig_unc")
    col_name <- c("space", "Estimate", "Sig", "With Uncertainty", "Sig_unc")
  }

  est_exr$space <- rep(" ", nrow(est_exr))
  est_exr <- est_exr[, col_name]
  colnames(est_exr)[1] <- colnames(est_exr)[3] <-  ""
  if(object$input$uncertainty == TRUE){
    colnames(est_exr)[5] <- ""
  }

  est_exr <- est_exr[order(est_exr$Estimate), , drop = FALSE]
  # est_exr_p <- est_exr[, c(2,3,4,5,6)]
  rownames(est_exr) <- object$input$cate_method


  # covariate profile
  mean_exp <- apply(object$input$X, 2, function(x) mean(x))
  mean_pop <- apply(object$input$X, 2,function(x) weighted.mean(x, w = object$w_combined[,1]))
  sd_exp <- apply(object$input$X, 2, function(x) sd(x))
  sd_pop <- apply(object$input$X, 2,function(x) weighted.sd(x, w = object$w_combined[,1]))
  std_diff <- (mean_exp - mean_pop)/sd_exp
  tab_X_orig <- tab_X <- cbind(mean_exp, sd_exp, mean_pop, sd_pop, std_diff)
  tab_X <- as.data.frame(round(tab_X, digits = digits))
  tab_X <- cbind(tab_X[,1:2], " ", tab_X[,3:4], " ", tab_X[,5])
  colnames(tab_X) <- c("Exp:Mean", "Exp:SD", "", "Pop*:Mean", "Pop*:SD","", "Std. Diff")

  if(length(object$input$cate_method) == 1){

    # Covariate Profile
    methods_used <- object$input$cate_method
    cov_prof <- cov_profile(X = object$input$X, w_use = object$w_combined[,1], digits = digits)
    tab_X <- cov_prof$tab_X; tab_X_orig <- cov_prof$tab_X_orig

    cat("\n")
    cat(paste0("CATE Estimator: ", methods_used, "\n\n"))
    cat("-------------------------\n")
    cat(paste0("External Robustness: ", round(est_exr$Estimate[1], 2), "\n"))
    cat("-------------------------\n\n")
    print(est_exr, row.names = FALSE)
    cat("---\n")
    cat(paste0("Note: 0 ' ' ", round(exr_nrs, 2), " (Probability Surveys)",
               " '*' ", round(exr_Mturk, 2), " (MTurk Samples)",
               " '**' 1"))
    cat("\n\n\n")
    cat("-------------------\n")
    cat(paste0("Covariate Profiles:\n"))
    cat("-------------------\n\n")
    print(tab_X)
    cat("---\n")
    cat(paste0("Note: Exp = Experimental sample. Pop* = Population with the T-PATE equal to zero."))
    if(object$est[1] == 1){
      cat(paste0("\nNote 2: When external robustness = 1, Pop* is NA because there is no population with the T-PATE equal to zero."))
    }

  }else{
    methods_used <- paste(object$input$cate_method, collapse = ", ")

    tab_X <- list()
    tab_X_orig <- list()
    for(z in 1:length(object$input$cate_method)){
      cov_prof <- cov_profile(X = object$input$X, w_use = object$w_combined[,z], digits = digits)
      tab_X[[z]] <- cov_prof$tab_X; tab_X_orig[[z]] <- cov_prof$tab_X_orig
    }

    cat("\n")
    cat(paste0("CATE Estimator: ", methods_used, "\n\n"))
    cat("-------------------------\n")
    cat(paste0("External Robustness: ", round(est_exr$Estimate[1], 2), "\n"))
    cat("-------------------------\n\n")
    print(est_exr, row.names = TRUE)
    cat("---\n")
    cat(paste0("Note: 0 ' ' ", round(exr_nrs, 2), " (Probability Surveys)",
               " '*' ", round(exr_Mturk, 2), " (MTurk Samples)",
               " '**' 1"))
    cat("\n\n\n")
    cat("-------------------\n")
    cat(paste0("Covariate Profiles:\n"))
    cat("-------------------\n\n")
    for(z in 1:length(object$input$cate_method)){
      cat(paste0("CATE Estimator: ",object$input$cate_method[z], "\n"))
      print(tab_X[[z]])
      cat("\n")
    }
    cat("---\n")
    cat(paste0("Note: Exp = Experimental sample. Pop* = Population with the T-PATE equal to zero."))
    if(any(object$est == 1)){
      cat(paste0("\nNote 2: When external robustness = 1, Pop* is NA because there is no population with the T-PATE equal to zero."))
    }

  }

  invisible(list("est" = est_exr, "covariate_profile" = tab_X_orig))
}
