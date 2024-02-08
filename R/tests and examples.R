library(dagitty)
library(xgboost)

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
data <- read.csv("C:/Users/christian.thorjussen/Bioco Case/BiocoData.csv", sep = ",")
features <- data[,c('TempDiff', 'AddedWater', 'Fat')]
label <-  data[,'Pressure']

data_matrix <- xgb.DMatrix(data = as.matrix(features), label = as.matrix(label))

best_max_depth <- NULL
best_nrounds <- NULL
best_test <- Inf
best_iteration <- NULL
max_depth <- c(1,2,3,4,5,6,7)
eta <- 0.1
n_folds = 10
nrounds = 100
for (depth in max_depth) {
  params <- list(
    eta = eta,
    max_depth = depth
  )
  for (fold in 1:n_folds) {
    fold_size <- floor(nrow(data_matrix) / n_folds)
    test_indices <- ((fold - 1) * fold_size + 1):(fold * fold_size)
    train_indices <- setdiff(1:nrow(data_matrix), test_indices)
    train_data <- data_matrix[train_indices, ]
    test_data <- data_matrix[test_indices, ]
    watchlist <- list(train = train_data, test = test_data)
    model <- xgboost::xgb.train(
      data = watchlist$train,
      objective = "reg:squarederror",
      max_depth = depth,
      eta =eta,
      nrounds = nrounds,
      early_stopping_rounds = 35,
      nthread = 1,
      watchlist = watchlist,
      verbose = FALSE
    )

      best_iteration <-  which.min(model$evaluation_log$test_rmse)
      min_cv <- model$evaluation_log$test_rmse[best_iteration]
    # Check if this combination has a lower RMSE than the previous best
    if (min_cv < best_test) {
      best_test <- min_cv
      best_max_depth <- depth
      best_nrounds <- best_iteration
    }
  }
}


# $P \ind TD | AW, F$
tic <- Sys.time()
bioco_test <-  BootyTest(formula = Pressure ~ TempDiff + AddedWater + Fat,
                         data = data,
                         max_depth = 3,
                         nrounds = 100,
                         early_stopping = 35,
                         statistic = xgboost_test,
                         p = 0.8,
                         nboot = 1500,
                         parallel = T)

toc <-  Sys.time()
toc - tic

bioco_test_plot <- plot.booty(your_booty = bioco_test, metric = 1)
bioco_test_plot
bioco_test$t[,1]
mean(bioco_test$t[,1], na.rm = TRUE)
sd(bioco_test$t[,1], na.rm = TRUE)
quantile(bioco_test$t[,1], na.rm = TRUE, probs = 0.025)
quantile(bioco_test$t[,1], na.rm = TRUE, probs = 0.975)




parallel_test <- BootyTest(data = data, dag = DAG, dag_cond = 2, statistic = bagging_test, p = 0.9, nboot = 100 )
hist(parallel_test$t[,1])
parallel_xgboosttest <- BootyTest(formula = X3 ~ X4 + X2 + X1, data = data, statistic = xgboost_test, p = 0.9, nboot = 200, parallel = T)
hist(parallel_xgboosttest$t[,1])

