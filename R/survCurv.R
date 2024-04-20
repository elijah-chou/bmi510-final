#' Calculate and Plot Survival Curve
#'
#' This function takes two numerical vectors \code{status} and \code{time} representing
#' survival status and survival time, respectively. It calculates and plots the survival curve S(t).
#'
#' @param status A numerical vector representing survival status (1 for event occurred, 0 for censored).
#' @param time A numerical vector representing survival time.
#' @return NULL. The function produces a plot of the survival curve.
#' @examples
#' # Load data
#' data <- read.csv("https://jlucasmckay.bmi.emory.edu/global/bmi510/Labs-Materials/survival.csv")
#' # Extract status and time vectors from the dataset
#' status <- data$status
#' time <- data$time
#' # Plot survival curve
#' survCurv(status, time)
#'
#' @export
survCurv <- function(status, time) {
  # Sort time and status vectors by time
  sorted_data <- data.frame(time = time, status = status)
  sorted_data <- sorted_data[order(sorted_data$time), ]

  # Calculate survival probabilities
  n <- length(time)
  S <- numeric(n)
  for (i in 1:n) {
    S[i] <- sum(sorted_data$status[i:n] == 0) / sum(sorted_data$status[i:n] >= 0)
  }

  # Plot survival curve
  plot(sorted_data$time, S, type = "l", xlab = "Time", ylab = "Survival Probability",
       main = "Survival Curve")
}
