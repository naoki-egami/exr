#' Visualize the distribution of the CATEs and estimated external robustness
#' @param x the output from exr()
#' @param cate_method When "grf", we use 'grf'. When "X-learner", we use 'X-learner' based on R package 'SuperLearner'.
#' @param uncertainty Logical. Whether we visualize the distribution of the CATEs and estimated external robustness based on an adjusted threshold that takes into account uncertainties.
#' @param ... Other arguments.
#' @export

plot.exr <- function(x, cate_method = NULL, uncertainty = FALSE, ...){

  # #################
  # Housekeeping
  # #################
  if(is.null(cate_method) == TRUE){
    m_p <- 1
    cate_method <- x$input$cate_method[1]
  }else{
    m_p <- which(x$input$cate_method == cate_method)
  }

  # HTE
  if(uncertainty == FALSE){
    exr_p <- round(x$est[m_p], 2)
    # xlab_name <- paste0("Estimates of CATE   (", cate_method, ")")
    xlab_name <- "Estimates of CATE"
    hte_use <- x$tau[, m_p]
    add <- ""
  }else{
    exr_p <- round(x$est_unc[m_p], 2)
    quant <- round(qnorm(1 - (1-x$input$ci)/2), 2)
    if(x$input$sign == 1){
      sign_use <- "- "
      hte_use <- x$tau[, m_p] - quant*x$se
    }else{
      sign_use <- "+ "
      hte_use <- x$tau[, m_p] + quant*x$se
    }
    xlab_name <- paste0("Estimates of CATE ", sign_use, quant, " * Standard Error")
    add <- "  (with uncertainty)"
  }

  # plotting
  hist(hte_use, breaks = 100,
       main = paste0("External Robustness: ", exr_p, add),
       xlab = xlab_name)
  abline(v = 0, lty = 2, lwd = 2)
}
