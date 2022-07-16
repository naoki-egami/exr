#' Adding Constraints
#' @param vars Names of variables that users want to add constraints.
#' @param type One of the three types; "between", "less than or equal to", "larger than or equal to".
#' @param values Values of the weighted mean of variables specified in `vars`. When `type = "between"`, `values` should be a vector of length 2. Otherwise, `values` should be one numeric value.
#' @export

constraint <- function(vars, type, values){
  if((type %in% c("between",
                  "less than or equal to",
                  "larger than or equal to")) == FALSE){
    stop(" 'type' in 'constraint' should be 'between', 'less than or equal to', or 'larger than equal to' ")
  }

  if(type == "larger than or equal to"){
    cons <- expression(sum(X[, vars] * w * orig_w) >= sum(w * orig_w)*values[1])
  }else if(type == "less than or equal to"){
    cons <- expression(sum(X[, vars] * w * orig_w) <= sum(w * orig_w)*values[1])
  }else if(type == "between"){
    cons <- list(expression(sum(X[, vars] * w * orig_w) >= sum(w * orig_w)*values[1]),
                 expression(sum(X[, vars] * w * orig_w) <= sum(w * orig_w)*values[2]))
  }
  # }else if(type == "equal to"){
  #   cons <- expression(sum(X[, vars] * w * orig_w) == sum(w * orig_w)*values[1])
  # }
  return(list("constraint" = cons, "vars" = vars, "values" = values, "type" = type))
}

KL_raking <- function(x){  # will remove
  if(is.null(x) == TRUE){
    return(Inf)
  }else if(any(x < 0)){
    return(Inf)
  }else{
    x0 <- x/mean(x)
    return(mean(x0*log(x0) - x0 + 1))
  }
}

trimming <- function(x, cut = -0.01){  # will remove
  if(is.null(x) == TRUE){
    return(x)
  }else{
    x0 <- x/mean(x)
    if(any(x0 < cut)){
      return(NULL)
    }else{
      x0[x0 <= 0] <- abs(cut)/10
      x02 <- x0/mean(x0)
      return(x02)
    }
  }
}

w_summary <- function(x, loss = "raking"){
  if(loss == "raking"){
    evv0 <- w_raking(x)
  }else if(loss == "KL"){
    evv0 <- w_KL(x)
  }else if(loss == "LS"){
    evv0 <- w_LS(x)
  }
  evv <- 1 - exp(-evv0)
  return(evv)
}

w_KL <- function(x){
  if(is.null(x) == TRUE){
    return(Inf)
  }else if(any(x < 0)){
    return(Inf)
  }else{
    x0 <- x/mean(x)
    return(mean(x0*log(x0)))
  }
}

w_raking <- function(x){
  if(is.null(x) == TRUE){
    return(Inf)
  }else if(any(x < 0)){
    return(Inf)
  }else{
    x0 <- x/mean(x)
    return(mean(x0*log(x0) - x0 + 1))
  }
}

w_LS <- function(x){
  if(is.null(x) == TRUE){
    return(Inf)
  }else if(any(x < 0)){
    return(Inf)
  }else{
    x0 <- x/mean(x)
    return(mean((x -1)^2))
  }
}

var_IPW <- function(Y, Tr, ps, w){
  score <- Tr*Y/ps - (1-Tr)*Y/(1-ps)
  var_i <- var(score*w)/length(Y)
  return(var_i)
}
