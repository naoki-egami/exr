#' Summary function
#' @param object the output from exr()
#' @param ... Other arguments.
#' @export

summary.exr <- function(object, ...){

  # #################
  # Housekeeping
  # #################
  # data(bench_prob_survey)
  exr_nrs <- 0.1437875
  # data(bench_mturk)
  exr_Mturk <- 0.5659691

  if(object$input$uncertainty == TRUE){
    est_exr <- data.frame(cbind(object$est, object$est_unc))
    colnames(est_exr) <- c("Estimate", "With CI")
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
  col_name <- c("Method", "space", "Estimate", "Sig")

  if(object$input$uncertainty == TRUE){
    sig_unc <- rep("", length(est_exr[, "With CI"]))
    sig_unc[est_exr[ , "With CI"]  > exr_Mturk] <- "**"
    sig_unc[est_exr[ , "With CI"]  >= exr_nrs & est_exr[ , "With CI"]  < exr_Mturk] <- "*"
    est_exr$Sig_unc <- sig_unc
    col_name <- c("Method", "space", "Estimate", "Sig", "With CI", "Sig_unc")
  }

  est_exr$space <- rep(" ", nrow(est_exr))
  est_exr <- est_exr[, col_name]
  colnames(est_exr)[2] <- colnames(est_exr)[4] <-  ""
  if(object$input$uncertainty == TRUE){
    colnames(est_exr)[6] <- ""
  }

  est_exr <- est_exr[order(est_exr$Estimate), , drop = FALSE]

  cat("\n")
  cat("-------------------------\n")
  cat(paste0("External Robustness: ", round(est_exr$Estimate[1], 2), "\n"))
  cat("-------------------------\n\n")
  print(est_exr, row.names = FALSE)
  cat("---\n")
  cat(paste0("Note: 0 ' ' ", round(exr_nrs, 2), " (Probability Surveys)",
             " '*' ", round(exr_Mturk, 2), " (MTurk Samples)",
             " '**' 1"))

  invisible(est_exr)
}
