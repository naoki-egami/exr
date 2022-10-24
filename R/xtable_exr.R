#' Function to produce a LaTeX output
#' @param object the output from exr()
#' @param digits digits used for a table of covariate profiles. Default = 2.
#' @param ... Other arguments.
#' @import xtable
#' @export

xtable.exr <- function(obj, digits = 2, ...){
  invisible(capture.output(main <- summary(obj)))
  X_use <- as.data.frame(round(main$covariate_profile, digits = digits))

  tab_X <- cbind(c(t(cbind(X_use[,1], paste0("(", X_use[,2], ")")))),
                 c(t(cbind(X_use[,3], paste0("(", X_use[,4], ")")))),
                 c(t(cbind(X_use[,5], NA))))
  tab_X_F <- rbind(c("Experimental", "Population with", "Standardized"),
                      c("Sample", "T-PATE = 0", "Difference"),
                      tab_X)
  tab_X_F <- cbind(c("","", t(cbind(rownames(X_use), ""))), tab_X_F)
  colnames(tab_X_F) <- NULL
  table_use <- xtable::xtable(tab_X_F)
  xtable::align(table_use) <- "ll||c|c||c"
  print(table_use,
        include.rownames=FALSE,
        include.colnames = FALSE,
        hline.after = c(2))
}

#' @export
xtable <- function(obj) {
  UseMethod("xtable")
}
