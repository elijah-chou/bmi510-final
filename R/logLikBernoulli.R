#' Calculate Maximum Likelihood Estimate of p for Bernoulli Distribution
#'
#' This function takes a vector of binary data and calculates the parameter \code{p}
#' that maximizes the log-likelihood function for a Bernoulli distribution.
#'
#' @param data A vector of binary data (0s and 1s).
#' @return The maximum likelihood estimate of \code{p}.
#' @examples
#' data <- c(1, 0, 0, 0, 1, 1, 1)
#' logLikBernoulli(data)
#'
#' @export
logLikBernoulli <- function(data) {
  # Grid search with step of 0.001
  ps <- seq(0, 1, by = 0.001)

  # Calculate log-likelihood for each value of p
  log_likelihoods <- sapply(ps, function(p) {
    sum(data * log(p) + (1 - data) * log(1 - p))
  })

  # Find index of maximum log-likelihood
  max_index <- which.max(log_likelihoods)

  # Return corresponding value of p
  ps[max_index]
}
