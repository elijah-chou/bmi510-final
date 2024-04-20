#' Principal Component Approximation
#'
#' This function takes a numerical vector \code{x} and returns an approximation
#' to the data based on a specified number of principal components (\code{npc}).
#' The approximation is rescaled and centered to match the original data.
#'
#' @param x A numerical vector representing the original data.
#' @param npc The number of principal components to use for approximation.
#' @return An approximation to the original data, rescaled and centered.
#' @examples
#' # Assuming pca_result is a result of PCA analysis
#' approx_data <- pcApprox(x, npc)
#'
#' @export
pcApprox <- function(x, npc) {
  pca_result <- prcomp(x, center = TRUE, scale. = TRUE)
  pcs <- pca_result$x[, 1:npc]
  approx_data <- pcs %*% t(pca_result$rotation[, 1:npc])
  if (npc < ncol(pca_result$rotation)) {
    approx_data <- approx_data * sqrt(pca_result$sdev[1:npc])
  }
  if (pca_result$center) {
    approx_data <- scale(approx_data, center = FALSE, scale = FALSE)
    approx_data <- approx_data + colMeans(x)
  }
  return(approx_data)
}
