tpate_boot <- function(x, w, Y, Tr, X, family, cate_method_use, lib, n, boot_ind, seed) {
  with_progress({
    out <- tpate_boot0(x = x,
                       w = w,
                       Y = Y, Tr = Tr, X = X, family = family,
                       cate_method_use = cate_method_use, lib = lib,
                       n = n, boot_ind = boot_ind, seed = seed)
  })
  return(out)
}

tpate_boot0 <- function(x, w, Y, Tr, X, family, cate_method_use, lib, n, boot_ind, seed) {
  p <- progressor(steps = length(x))

  future_map(x, function(i) {
    p()
    # p(message = sprintf("%g", x[i]), class = "sticky")
    tryCatch({
      cate_base(x = i, weights = w,
                Y = Y, Tr = Tr, X = X, family = family,
                cate_method_use = cate_method_use, lib = lib,
                n = n, boot_ind = boot_ind, seed = seed)
    }, error = function(e) {
      NA
    })
  }, .options = furrr_options(seed = TRUE))
}

cate_base <- function(x, weights,
                      Y, Tr, X, family,
                      cate_method_use, lib,
                      n, boot_ind, seed){

  # Bootstrap the data
  boot_use <- boot_data(x = x, seed = seed, n = n, boot_ind = boot_ind)

  # fit
  fit_tau <- tau_fit(Y = Y[boot_use],
                     Tr = Tr[boot_use],
                     X = X[boot_use, , drop = FALSE],
                     family = family,
                     new_X = X[boot_use, ,drop = FALSE],
                     cate_method = cate_method_use, lib = lib, seed = seed)
  weights_new <- weights[boot_use]

  out <- sum(fit_tau$test_tau*weights_new)/sum(weights_new)
  return(out)
}

boot_data <- function(x, seed, n, boot_ind){

  # boot_ind should be the same length as data

  # set seed
  seed.b <- 1000*x + seed
  set.seed(seed.b)

  if(is.null(boot_ind) == FALSE){
    # Block Bootstrap (within each treatment)
    boot_id0 <- sample(unique(boot_ind), size = length(unique(boot_ind)), replace = TRUE)
    # # create bootstap sample with sapply
    boot_which <- sapply(boot_id0, function(x) which(boot_ind == x))
    boot_use <- unlist(boot_which)
  }else if(is.null(boot_ind) == TRUE){
    boot_use <- sample(1:n, size = n, replace = TRUE)
  }

  return(boot_use)
}
