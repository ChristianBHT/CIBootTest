
BootyTest <- function(data = data, formula = NULL, statistic, nboot = 25,
                       bootstrap_sample = TRUE, bayes = TRUE, p = NULL,
                       alpha = c(rep(1,nrow(data))), dag = NULL, dag_cond = NULL,
                       parallel = NULL, ncores, ...){
  if (is.null(parallel)){
    if (bootstrap_sample) {
      if (bayes) {
        if (!is.null(alpha)) {
          # Check length of alpha
          if (length(alpha) != nrow(data)) {
            stop("Error: the prior alpha should have the same number of entries as the rows in data")
          }
        }
        weights <- DirichletReg::rdirichlet(nboot, alpha)
        output <- boot::boot(data = data, statistic = function(data, indices,...) statistic(data, indices, formula , p, ...), R = rep(1,nboot), weights = weights)
      } else {
        output <- boot::boot(data = data, statistic = function(data, indices,...) statistic(data, indices, formula , p, ...), R = nboot)
      }
    } else {
      if (statistic == bagging_test){
        output <- bagging_test(data, indices = NULL, formula, p, nboot)
      } else if (statistic == xgboost_test) {
        output <- xgboost_test(data, indices = NULL, formula, p)
      }
    }
  } else if (!is.null(parallel)){

}
  return(output)
}


