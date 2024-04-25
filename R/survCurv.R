#' Calculate and Plot Survival Curve
#'
#' This function takes two numerical vectors \code{status} and \code{time} representing
#' survival status and survival time, respectively. It calculates and plots the survival curve S(t).
#'
#' @param status A numerical vector representing survival status (1 for event occurred, 0 for censored).
#' @param time A numerical vector representing survival time.
#' @return NULL. The function produces a plot of the survival curve.
#' @importFrom graphics lines
#' @examples
#' data <- read.csv("https://jlucasmckay.bmi.emory.edu/global/bmi510/Labs-Materials/survival.csv")
#' survCurv(data$status, data$time)
#'
#' @export
survCurv <- function(status, time) {
  data <- data.frame(status = status, time = time)
  data <- data[order(data$time), ]
  n <- length(status)
  surv_prob <- rep(1, n)
  events <- rep(0, n)
  for (i in 1:n) {
    events[i] <- sum(data$time[i] >= data$time & data$status == 1)
    surv_prob[i] <- (n - events[i]) / n
  }

  survival_curve <- data.frame(time = data$time, surv_prob = surv_prob)
  plot(survival_curve$time, survival_curve$surv_prob, type = "s", ylim = c(0, 1),
       xlab = "Time", ylab = "Survival Probability", main = "Survival Curve")
  graphics::lines(survival_curve$time, survival_curve$surv_prob, type = "s")
}
