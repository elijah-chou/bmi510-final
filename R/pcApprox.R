#' Principal Component Approximation
#'
#' This function takes a numerical vector \code{x} and returns an approximation
#' to the data based on a specified number of principal components (\code{npc}).
#' The approximation is rescaled and centered to match the original data.
#'
#' @param x A numerical vector representing the original data.
#' @param npc The number of principal components to use for approximation.
#' @return An approximation to the original data, rescaled and centered.
#' @importFrom stats prcomp
#' @importFrom MASS mvrnorm
#' @examples
#' # Generate sample data
#' set.seed(123)
#' n <- 100
#' p <- 5
#' Sigma <- matrix(0.5, p, p)
#' diag(Sigma) <- 1
#' data <- MASS::mvrnorm(n, rep(0, p), Sigma)
#'
#' # Perform principal component analysis approximation
#' approx_data <- pcApprox(data, npc = 2)
#' head(approx_data)
#'
#' @export
pcApprox <- function(x, npc) {
  pca_result <- stats::prcomp(x)
  approx <- pca_result$x[, 1:npc] %*% t(pca_result$rotation[, 1:npc])
  approx <- approx * sqrt(pca_result$sdev[1:npc])
  approx <- approx + colMeans(x)
  return(approx)
}
