#' Reverse Scaling and Centering
#'
#' This function takes a vector that has been previously scaled (e.g., using the \code{scale()} function)
#' and reverses the centering and scaling.
#'
#' @param x A numerical vector that has been previously scaled.
#' @return The original unscaled vector.
#' @examples
#' scaled_vector <- scale(1:10)
#' unscaled_vector <- unscale(scaled_vector)
#'
#' @export
unscale <- function(x) {
  unscaled <- x * attr(x, 'scaled:scale') + attr(x, "scaled:center")
  attr(unscaled, 'scaled:scale') <- NULL
  attr(unscaled, 'scaled:center') <- NULL
  return(unscaled)
}
