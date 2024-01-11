BootyTest <- function(data = data, formula = NULL, statistic, nboot = 25,
                       bootstrap_sample = TRUE, bayes = TRUE, p = NULL,
                       alpha = c(rep(1,nrow(data))), dag = NULL, dag_cond = NULL,
                       parallel = NULL, ncores, ...){
  if (is.null(data)) {
    stop("No data, no testing")
  }
  if (is.null(p)) {
    stop("Please provide the parameter p (size of training set)")
  }
  if (p <= 0 || p >= 1) {
    stop("You silly you, p must be between 0 and 1")
  }
  if (is.null(formula) & is.null(dag)) {
    stop("Please provide a R-formula for the condition to be tested, i.e. Y _||_ X | Z -> Y ~ X + Z.
         Or provide a dagitty DAG with the option: dag = 'your DAG' and dag_cond = '# of the condition in need of testing'")
  } else if (!(class(formula) %in% "formula") & !is.null(formula)) {
    formula <- as.formula(formula)
  } else if (is.null(formula) & !is.null(dag)) {
    # Use provided DAG and extract the testable condition
    if (!class(dag) %in% 'dagitty') {
      stop('The provided DAG needs to be a dagitty object')
    }
    characters <- unlist(dagitty::impliedConditionalIndependencies(dag)[dag_cond])
    formula_str <- paste(characters[2:length(characters)], collapse = " + ")
    formula_dep <- characters[1]
    formula <- as.formula(paste(formula_dep ,"~", formula_str))
    }

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
    if (bootstrap_sample) {
      if (bayes) {
        if (!is.null(alpha)) {
          # Check length of alpha
          if (length(alpha) != nrow(data)) {
            stop("Error: the prior alpha should have the same number of entries as the rows in data")
          }
        }
        weights <- DirichletReg::rdirichlet(nboot, alpha)
        # Applying the parallel = "snow" option, which is only valid for windows computers
        output <- boot::boot(data = data, statistic = function(data, indices,...) statistic(data, indices, formula , p, ...), R = rep(1,nboot), weights = weights, parallel = 'snow')
      } else {
        output <- boot::boot(data = data, statistic = function(data, indices,...) statistic(data, indices, formula , p, ...), R = nboot, parallel = 'snow')
      }
    } else {
      if (statistic == bagging_test){
        output <- bagging_test(data, indices = NULL, formula, p, nboot)
      } else if (statistic == xgboost_test) {
        output <- xgboost_test(data, indices = NULL, formula, p)
      }
    }
  }
  print(formula)
  return(output)
}
BootyTest(data = data, formula = NULL, dag = DAG, dag_cond = 2, statistic = bagging_test, p = 0.7, nboot = 10)
BootyTest(data = data, formula = X3 ~ X4 + X2 + X1, statistic = xgboost_test, p = 0.7, nboot = 10)


