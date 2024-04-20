#' Calculate Minimum Sample Size for T-Test
#'
#' This function acts as a wrapper around pwr::pwr.t2n.test to calculate the minimum sample size
#' needed for a t-test with the given preliminary data, ensuring 80% power at alpha=0.05.
#' In the two sample case, it will return the minimum sample size for the second sample.
#'
#' @param x1 A numeric vector representing the first sample.
#' @param x2 Optional. A numeric vector representing the second sample. If omitted, only x1 is used.
#' @return The minimum sample size needed for a t-test.
#' @importFrom pwr pwr.t.test pwr.t2n.test
#' @importFrom stats sd
#' @examples
#' # One sample case
#' x1 <- rnorm(90, mean = -3, sd = 10)
#' minimumN(x1)
#'
#' # Two sample case
#' x2 <- rnorm(50, mean = 3, sd = 10)
#' minimumN(x1, x2)
#'
#' @export
minimumN <- function(x1, x2 = NULL) {
  if (is.null(x2)) {
    result <- pwr::pwr.t.test(d = mean(x1) / sd(x1), sig.level = 0.05, power = 0.8, n = NULL, alternative = "two.sided")$n
  } else {
    result <- pwr::pwr.t2n.test(d = abs(mean(x1) - mean(x2)) / sqrt((sd(x1)^2 + sd(x2)^2) / 2), n1 = length(x1), n2 = NULL,
                                sig.level = 0.05, power = 0.8, alternative = "two.sided")$n2
  }
  return(ceiling(result))
}
