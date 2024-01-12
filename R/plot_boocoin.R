#' BootyPlot
#' @description
#' A short description...
#'
#' @param your_booty
#' @param metric Select which metric you want, 1 for RMSE or Accuracy, 2 for R-squared or Kappa Score
#'
#' @return
#' @export
#'
#' @examples

plot.booty <- function(your_booty, metric = 1) {

    Metric <- names(your_booty$t0[metric])
    results <- your_booty$t[,metric]
    df <- data.frame(Value = results)
    boots <- your_booty$R

    ggplot2::ggplot(df, ggplot2::aes(x = Value)) +
      ggplot2::geom_histogram(ggplot2::aes(y = ..density..), binwidth = 0.01, fill = "skyblue", color = "black") +
      ggplot2::geom_density(ggplot2::aes(color = "Density"), size = 1, color = 'black') +
      ggplot2::labs(
        title = paste("BootyTest Results, ", boots, " bootstraps"),
        x = paste("",Metric),
        y = "Density"
      ) +
      ggplot2::theme_minimal()
}

my_booty <- BootyTest(formula = X3 ~ X4 + X2 + X1, data = data, statistic = bagging_test, p = 0.9, nboot = 400, bayes = TRUE)
RMSE_plot <- plot.booty(your_booty = my_booty, metric = 1)
R2_plot <- plot.booty(your_booty = my_booty, metric = 2)




