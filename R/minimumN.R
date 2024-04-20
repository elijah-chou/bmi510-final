#' Calculate Minimum Sample Size for T-Test
#'
#' This function acts as a wrapper around pwr::pwr.t2n.test to calculate the minimum sample size
#' needed for a t-test with the given preliminary data, ensuring 80% power at alpha=0.05.
#'
#' @param x1 A numeric vector representing the first sample.
#' @param x2 Optional. A numeric vector representing the second sample. If omitted, only x1 is used.
#' @return The minimum sample size needed for a t-test.
#' @importFrom pwr pwr.t2n.test
#' @examples
#' # One sample case
#' x1 <- rnorm(20)
#' minimumN(x1)
#'
#' # Two sample case
#' x2 <- rnorm(25)
#' minimumN(x1, x2)
#'
#' @export
minimumN <- function(x1, x2 = NULL) {
  if (is.null(x2)) {
    result <- pwr::pwr.t2n.test(d = mean(x1) / sd(x1), sig.level = 0.05, power = 0.8, alternative = "two.sided")$n
  } else {
    result <- pwr::pwr.t2n.test(h = abs(mean(x1) - mean(x2)) / sqrt((sd(x1)^2 + sd(x2)^2) / 2),
                                sig.level = 0.05, power = 0.8, alternative = "two.sided")$n
  }
  return(ceiling(result))
}
