data_gen <- function(N){
  X1 <- rnorm(N,0,1)
  X2 <- rnorm(N,0,1)
  X3 <- rnorm(N,X1*X2,0.5)
  X4 <- rnorm(N,X1*X2,0.5)
  df <- data.frame(X1,X2,X3,X4)
  return(df)
}
data <- data_gen(2000)

BootyTest(data = data, formula = X3 ~ X4 + X2 + X1, statistic = bagging_test, p = 0.7, nboot = 250, parallel = TRUE)

library(dagitty)
DAG <- dagitty('dag {
bb="0,0,1,1"
X1 [pos="0.022,0.032"]
X2 [pos="0.024,0.128"]
X3 [pos="0.118,0.034"]
X4 [pos="0.116,0.134"]
X1 -> X3
X1 -> X4
X2 -> X3
X2 -> X4
}
')
model6 <- function(N){
  X1 = rnorm(N,1,1)
  X2 = rnorm(N,0,1)
  X3 = rnorm(N,exp(X2*X1),1)
  X4 = rnorm(N,X2*X1,1)
  df <- data.frame(X1,X2,X3,X4)
  return(df)
}

parallel_test <- BootyTest(data = data, dag = DAG, dag_cond = 2, statistic = bagging_test, p = 0.9, nboot = 100 )
hist(parallel_test$t[,1])
parallel_xgboosttest <- BootyTest(formula = X3 ~ X4 + X2 + X1, data = data, statistic = xgboost_test, p = 0.9, nboot = 200, parallel = T)
hist(parallel_xgboosttest$t[,1])

