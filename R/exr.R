#' Estimating External Robustness
#' @param outcome Name of the outcome variable.
#' @param treatment Name of the treatment variable. The treatment variable needs to be a binary variable.
#' @param covariates Name of the covariates
#' @param data `data` needs to be `data.frame`
#' @param sate_estimate A vector of length 2; a point estimate of the SATE and its standard error. Default = NULL. When NULL, the package internally estimates the SATE using a linear regression of the outcome on the treatment and all specified covariates.
#' @param family "gaussian" (continuous outcomes) or "binomial" (binary outcomes). Default = `gaussian`.
#' @param threshold Numeric. This is the threshold for the T-PATE. Default = 0
#' @param cate_method When "grf", we use 'grf'. When "X-learner", we use 'X-learner' based on R package 'SuperLearner'.
#' @param cate_name Name of columns in `data` that store CATEs estimated outside the function. Default = NULL, and the CATE is internally estimated by a method specified by `cate_method.`
#' @param uncertainty Logical. Whether we take into account uncertainties to estimate external robustness. Default is TRUE.
#' @param ci Numeric. Specify the level of the confidence interval. Default = 0.95. Only used when `uncertainty = TRUE`.
#' @param boot Logical. When `boot = TRUE`, we use bootstrap to approximate confidence intervals. When `boot = FALSE`, we approximate the standard error using the standard error of the SATE (This is computationally much faster, so researchers can use this as an initial check. But for final results, we recommend `boot = TRUE`).
#' @param nboot Numeric. The number of bootstrap. Default = 100. Only used when `uncertainty = TRUE` and `boot = TRUE`.
#' @param clusters Vector. Unique identifiers for computing cluster standard errors. Only used when `uncertainty = TRUE` and `boot = TRUE`.
#' @param numCores Numeric. Default = 1. Number of cores to use. Parallel computing based on `future` package is used only when `uncertainty = TRUE` and `boot = TRUE`. When `numCores = NULL`, it automatically detects the number of available cores.
#' @param const_list List. Constraints to incorporate partial knowledge about population data. See Examples.
#' @param lib (optional) The library used for 'SuperLearner' when we choose `cate_method = "X-learner"`. Default = c("mean", "glm", "ranger").
#' @param verbose Logical. Whether to see outputs from the underlying optimization package `CVXR`. Default = FALSE.
#' @param seed Numeric. `seed` used internally. Default = 1234.
#' @import SuperLearner
#' @import CVXR
#' @import grf
#' @import bartCause
#' @import future.apply
#' @import progressr
#' @import furrr
#' @importFrom parallel detectCores
#' @importFrom future plan multisession sequential
#' @importFrom graphics abline hist
#' @importFrom stats as.formula lm model.frame model.matrix model.response na.omit predict qnorm sd var
#' @importFrom utils data
#' @return \code{exr} returns an object of \code{exr} class.
#'  \itemize{
#'    \item \code{est}: Estimated external robustness.
#'    \item \code{est_unc}: Estimates of the pAMCE for all factors in each bootstrap sample.
#'    \item \code{tau}: Estimated conditional average treatment effects (CATEs) for each unit.
#'    \item \code{w}: Estimated weights that solves the KL minimization problem where a constraint focuses on a point estimate of the T-PATE.
#'    \item \code{w_unc}: Estimated weights that solves the KL minimization problem where a constraint focuses on a confidence internal of the T-PATE.
#'    \item \code{...}: Values for internal use.
#'  }
#' @references Devaux and Egami. (2022+). Quantifying Robustness to External Validity Bias. Available at \url{https://naokiegami.com/paper/external_robust.pdf}.
#' @export

exr <- function(outcome, treatment, covariates,
                data,
                sate_estimate = NULL,
                family = "gaussian",
                threshold = 0,
                cate_method = "grf",
                cate_name = NULL,
                uncertainty = TRUE, ci = 0.95,
                boot = FALSE, nboot = 100, clusters = NULL,
                numCores = 1,
                const_list = NULL,
                lib = c("mean", "glm", "ranger"),
                verbose = FALSE,
                seed = 1234){

  # ##################
  # Housekeeping
  # ##################

  # For now, we use the following setup
  loss <- "KL"
  tolerance <- 1e-05
  solver <- "ECOS"
  dual <- TRUE
  cut <- -0.01

  # data.frame
  class(data) <- "data.frame"

  # Check Treatment Variables
  if(length(unique(na.omit(data[, treatment]))) != 2){
    stop(" 'treatment' variable needs to be binary (i.e., contains two levels). If users are interested in categorical treatments, they can consider each level separately. ")
  }

  # Number of cores
  if(is.null(numCores) == FALSE){
    if(numCores >= detectCores()) numCores <- detectCores() - 2
  }
  if(is.null(numCores)) numCores <- detectCores() - 2

  # check constraints
  const_vars <- unlist(lapply(const_list, function(x) x$vars))
  ## Check they are in covariates
  if(all(is.element(const_vars, covariates)) == FALSE){
    stop(" 'vars' in 'const_list' should be in  'covariates'. Please check `covariates`. ")
  }
  ## Check they are not factors
  fac_vars <- covariates[unlist(lapply(data[, covariates], function(x) class(x) == "factor"))]
  if(any(is.element(const_vars, fac_vars))){
    stop(" 'vars' in 'const_list' are 'factor'. Please change it to `numeric` in `data`. ")
  }

  # boot does not work for external CATE
  if(is.null(cate_name) == FALSE){
    if(uncertainty == TRUE & boot == TRUE){
      stop(" Bootstrap-based uncertainty does not work with CATE estimated outside the function using `cate_name`. " )
    }
  }else{
    cate_external <- NULL
  }

  # clean data
  data <- data[, c(outcome, treatment, covariates, cate_name)]
  for(j in 1:ncol(data)){
    if("factor" %in% class(data[,j])){
      data[,j] <- droplevels(data[,j])
      levels(data[,j]) <- gsub(" ", "", levels(data[,j]))
      levels(data[,j]) <- gsub("-", ".", levels(data[,j]))
    }
  }
  # Estimate SATE if not provided.
  if(is.null(sate_estimate) == TRUE){
    sate_for <- paste0(outcome, "~", paste(c(treatment, covariates), collapse = "+"))
    sate_estimate <- summary(lm(sate_for, data = data))$coef[treatment, c(1,2)]
  }
  if(sate_estimate[1] > threshold){
    sign <- 1
  }else if(sate_estimate[1] < threshold){
    sign <- -1
  }else{
    if(length(sate_estimate) != 2){
      stop(" `sate_estimate` should be a vector of length 2. The first element is the SATE estimate and the second is its standard error.")
    }
  }

  # #####################
  # Setup
  # #####################
  formula   <- as.formula(paste0(outcome, "~", paste(c(treatment, covariates, cate_name), collapse = "+")))
  formula_X <- as.formula(paste0("~", paste(c(covariates), collapse = "+")))

  mf <- model.frame(formula, data = data)
  Y  <- model.response(mf)
  Tr <- mf[, treatment]
  X_name <- colnames(model.matrix(formula_X, data = data)) # this is more robust way to handle categorical variables
  X  <- model.matrix(formula, data = data)[, X_name[-1]] # this is more robust way to handle categorical variables
  X  <- X[, (apply(X, 2, var) > 0), drop = FALSE]
  if(is.null(cate_name) == FALSE){
    cate_external <- mf[, cate_name, drop = FALSE]
  }else{
    cate_external <- NULL
  }

  # compute EXR
  out <- exr_base(Y = Y,
                  Tr = Tr,
                  X = X,
                  family = family,
                  cate_method = cate_method,
                  cate_external = cate_external,
                  uncertainty = uncertainty,
                  sate_estimate = sate_estimate,
                  boot = boot,
                  nboot = nboot,
                  const_list = const_list,
                  ci = ci,
                  lib = lib,
                  numCores = numCores,
                  clusters = clusters,
                  threshold = threshold,
                  sign = sign,
                  loss = loss,
                  cut = cut,
                  verbose = verbose,
                  tolerance = tolerance,
                  solver = solver,
                  seed = seed,
                  dual = dual)

  input <- list("cate_method" = cate_method,
                "sign" = sign,
                "X" = X,
                "uncertainty" = uncertainty,
                "ci" = ci,
                "sign" = sign)

  output <- c(out, list("input" = input))

  class(output)  <- c(class(output), "exr")
  return(output)
}
