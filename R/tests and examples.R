data_gen <- function(N){
  X1 <- rnorm(N,0,1)
  X2 <- rnorm(N,0,1)
  X3 <- rnorm(N,X1*X2,1)
  X4 <- rnorm(N,X1*X2,1)
  df <- data.frame(X1,X2,X3,X4)
  return(df)
}
data <- data_gen(2000)

BootyTest(data = data, formula = X3 ~ X4 + X2 + X1, statistic = bagging_test, p = 0.7, nboot = 250, parallel = TRUE)

library(dagitty)

dag <- dagitty('dag {
  A [pos="-2.200,-1.520"]
  B [pos="1.400,-1.460"]
  D [outcome,pos="1.400,1.621"]
  E [exposure,pos="-2.200,1.597"]
  Z [pos="-0.300,-0.082"]
  A -> E
  A -> Z
  B -> D
  B -> Z
  E -> D
  Z -> D
  Z -> E
}')


test_cond <- dagitty::impliedConditionalIndependencies(dag)
test_cond
data <- data_gen(2000)

simpletest <- BootyTest(data = data, statistic = bagging_test, p = 0.9, formula = X4~X3 + X1 + X2, nboot = 300)
hist(simpletest$t[,1])


statistic = xgboost_test
formula = X3 ~ X4 + X2 + X1
p = 0.7
DAG <- dagitty('dag {
  bb="0,0,1,1"
  "X1" [pos="0.026,0.039"]
  X2 [pos="0.029,0.163"]
  X3 [pos="0.155,0.037"]
  X4 [pos="0.154,0.164"]
  "X1" -> X3
  "X1" -> X4
  X2 -> X3
  X2 -> X4
}')
class(DAG)
dag_cond <- 2
dag = DAG
formula = NULL
