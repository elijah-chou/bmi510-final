#' Reverse Scaling and Centering
#'
#' This function takes a vector that has been previously scaled (e.g., using the \code{scale()} function)
#' and reverses the centering and scaling.
#'
#' @param x A numerical vector that has been previously scaled.
#' @return The original unscaled vector.
#' @examples
#' scaled_data <- scale(data)
#' unscaled_data <- unscale(scaled_data)
#'
#' @export
unscale <- function(x) {
  unscaled <- x * attr(x, 'scaled:scale') + attr(x, "scaled:center")
  return(unscaled)
}
