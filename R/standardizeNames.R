#' Standardize Variable Names
#'
#' This function takes a tibble \code{data} and standardizes the variable names to "small_camel" case
#' using \code{dplyr::rename_with} and \code{janitor::make_clean_names}.
#'
#' @param data A tibble with variable names to be standardized.
#' @return A tibble with standardized variable names.
#' @importFrom dplyr rename_with
#' @importFrom janitor make_clean_names
#' @examples
#' # Load required libraries
#' library(dplyr)
#' library(janitor)
#' # Create sample tibble
#' data <- tibble(`First Name` = c("Alice", "Bob", "Charlie"),
#'                `Last Name` = c("Smith", "Johnson", "Doe"))
#' # Standardize variable names
#' standardized_data <- standardizeNames(data)
#'
#' @export
standardizeNames <- function(data) {
  new_names <- janitor::make_clean_names(names(data))
  data <- dplyr::rename_with(data, ~new_names)
  return(data)
}
