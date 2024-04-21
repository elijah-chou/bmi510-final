#' Calculate and Plot Survival Curve
#'
#' This function takes two numerical vectors \code{status} and \code{time} representing
#' survival status and survival time, respectively. It calculates and plots the survival curve S(t).
#'
#' @param status A numerical vector representing survival status (1 for event occurred, 0 for censored).
#' @param time A numerical vector representing survival time.
#' @return NULL. The function produces a plot of the survival curve.
#' @importFrom ggsurvfit survfit2 ggsurvfit
#' @importFrom survival Surv
#' @importFrom ggplot2 labs
#' @examples
#' data <- read.csv("https://jlucasmckay.bmi.emory.edu/global/bmi510/Labs-Materials/survival.csv")
#' survCurv(data$status, data$time)
#'
#' @export
# survCurv <- function(status, time) {
#   sorted_data <- data.frame(time = time, status = status)
#   sorted_data <- sorted_data[order(sorted_data$time), ]
#   n <- length(time)
#   S <- numeric(n)
#   for (i in 1:n) {
#     S[i] <- sum(sorted_data$status[i:n] == 0) / sum(sorted_data$status[i:n] >= 0)
#   }
#   plot(sorted_data$time, S, type = "l", xlab = "Time", ylab = "Survival Probability",
#        main = "Survival Curve")
# }
survCurv <- function(status, time) {
  ggsurvfit::survfit2(survival::Surv(time, status) ~ 1) |>
    ggsurvfit::ggsurvfit() +
    ggplot2::labs(
      x = "Time",
      y = "Overall survival probability"
    )
}
