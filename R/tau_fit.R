tau_fit <- function(Y, Tr, X, family = "gaussian",
                    new_X,
                    cate_method = "T-learner",
                    lib = c("glm", "ranger"), seed = 1234){

  detail <- NULL

  if(cate_method %in% c("X-learner", "T-learner", "S-learner")){

    # library
    # lib <- unique(c("mean", lib))
    sl_lib <- paste0("SL.", lib)

    if(cate_method == "S-learner"){
      XT_g <- data.frame(Tr, X)
      set.seed(seed)
      suppressWarnings({fit <- SuperLearner::SuperLearner(Y = Y, X = XT_g,
                                                          SL.library = sl_lib, family = family)})
      X1 <- data.frame(cbind(1, new_X)); colnames(X1)[1] <- "Tr"
      X0 <- data.frame(cbind(0, new_X)); colnames(X0)[1] <- "Tr"
      suppressWarnings({Y1_h <- predict(fit, newdata = X1,
                                        Y = Y, X = XT_g, onlySL = TRUE)$pred})
      suppressWarnings({Y0_h <- predict(fit, newdata = X0,
                                        Y = Y, X = XT_g, onlySL = TRUE)$pred})
      # bca <- Y0_h
      suppressWarnings({tau <- Y1_h - Y0_h})
      table_sl <- cbind(fit$cvRisk, fit$coef)
      rm(fit)
    }else if(cate_method == "T-learner"){
      X <- data.frame(X)
      set.seed(seed)
      suppressWarnings({fit_1 <- SuperLearner::SuperLearner(Y = Y[Tr == 1], X = X[Tr == 1, , drop = FALSE],
                                                            SL.library = sl_lib, family = family)})
      suppressWarnings({fit_0 <- SuperLearner::SuperLearner(Y = Y[Tr == 0], X = X[Tr == 0, , drop = FALSE],
                                                            SL.library = sl_lib, family = family)})
      suppressWarnings({Y1_h <- predict(fit_1, newdata = data.frame(new_X),
                                        Y = Y[Tr == 1], X = X[Tr == 1, , drop = FALSE], onlySL = TRUE)$pred})
      suppressWarnings({Y0_h <- predict(fit_0, newdata = data.frame(new_X),
                                        Y = Y[Tr == 0], X = X[Tr == 0, , drop = FALSE], onlySL = TRUE)$pred})
      # bca <- Y0_h
      suppressWarnings({tau <- Y1_h - Y0_h})

      table_sl <- cbind((fit_1$cvRisk + fit_0$cvRisk)/2, (fit_1$coef + fit_0$coef)/2)
      rm(fit_1); rm(fit_0)
    }else if(cate_method == "X-learner"){
      X <- data.frame(X)
      set.seed(seed)
      X_1 <- X[Tr == 1, , drop = FALSE]
      X_0 <- X[Tr == 0, , drop = FALSE]

      suppressWarnings({fit_1 <- SuperLearner::SuperLearner(Y = Y[Tr == 1], X = X_1,
                                                            SL.library = sl_lib, family = family)})
      suppressWarnings({fit_0 <- SuperLearner::SuperLearner(Y = Y[Tr == 0], X = X_0,
                                                            SL.library = sl_lib, family = family)})
      suppressWarnings({Y1_in <- predict(fit_1, newdata = X_0, Y = Y[Tr == 1], X = X_1, onlySL = TRUE)$pred})
      suppressWarnings({Y0_in <- predict(fit_0, newdata = X_1, Y = Y[Tr == 0], X = X_0, onlySL = TRUE)$pred})

      suppressWarnings({tau_in_1 <- Y1_in - Y[Tr == 0]})
      suppressWarnings({tau_in_0 <- Y[Tr == 1] - Y0_in})

      rm(fit_1); rm(fit_0)

      suppressWarnings({fit_X <- SuperLearner::SuperLearner(Y = c(tau_in_1, tau_in_0), X = rbind(X_0, X_1),
                                                            SL.library = sl_lib, family = "gaussian")})

      suppressWarnings({tau <- predict(fit_X, newdata = data.frame(new_X),
                                       Y = c(tau_in_1, tau_in_0), X = rbind(X_0, X_1), onlySL = TRUE)$pred})

      table_sl <- cbind(fit_X$cvRisk, fit_X$coef)
      rm(fit_X)
    }

    # add detail
    colnames(table_sl) <- c("Risk", "Coef")

    detail <- table_sl
  }else if(cate_method == "grf"){
    Y <- as.numeric(Y); Tr <- as.numeric(Tr)
    fit <- causal_forest(X = X, Y = Y, W = Tr, seed = seed)
    # forest_Y0 <- regression_forest(X = X[Tr == 0, , drop = FALSE], Y = Y[Tr == 0], seed = seed)
    tau <- predict(fit, newdata = new_X)$predictions
    # bca <- predict(forest_Y0, newdata = new_X)$predictions
    remove(fit)
    # train_tau <- predict(fit)$predictions
  }else if (cate_method == "lm"){
    d1 <- data.frame(Y, X)[Tr == 1, ]
    d0 <- data.frame(Y, X)[Tr == 0, ]
    lm_1 <- lm(Y ~ ., data = d1)
    lm_0 <- lm(Y ~ ., data = d0)
    Y1_h <- cbind(1, new_X) %*% lm_1$coefficients
    Y0_h <- cbind(1, new_X) %*% lm_0$coefficients
    # bca <- Y0_h
    tau <- Y1_h - Y0_h
    detail <- list("lm_1_coef" = lm_1$coefficients,
                   "lm_0_coef" = lm_0$coefficients)
  }else if(cate_method == "bartCause"){
    bart_fit <- bartc(response = Y,
                      treatment = Tr,
                      confounders = X,
                      method.trt = "none",
                      keepTrees = TRUE)
    Y1_h <- t(predict(bart_fit, newdata = new_X, type = "y.1"))
    Y0_h <- t(predict(bart_fit, newdata = new_X, type = "y.0"))
    tau <- apply((Y1_h - Y0_h), 1, mean)
    rm(bart_fit)
  }
  out <- list("test_tau" = as.numeric(tau), "detail" = detail)
  return(out)
}
