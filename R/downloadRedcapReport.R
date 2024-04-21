#' Download RedCap Report
#'
#' This function queries a RedCap report using the provided API token, URL, and report ID,
#' and returns the contents as a tibble.
#'
#' @param redcapTokenName The name of the API token stored in .REnviron.
#' @param redcapUrl The URL of the RedCap instance.
#' @param redcapReportId The ID of the RedCap report to query.
#' @return A tibble containing the contents of the RedCap report.
#' @importFrom httr POST content status_code
#' @importFrom tibble tibble
#' @examples
#' # Example usage
#' redcap_report <- downloadRedcapReport("redcap_token", "https://redcap.emory.edu/api/", "46524")
#' @export
downloadRedcapReport <- function(redcapTokenName, redcapUrl, redcapReportId) {
  api_token <- Sys.getenv(redcapTokenName)
  print(api_token)
  formData <- list("token"=api_token,
                   content='report',
                   format='csv',
                   report_id=redcapReportId,
                   csvDelimiter='',
                   rawOrLabel='raw',
                   rawOrLabelHeaders='raw',
                   exportCheckboxLabel='false',
                   returnFormat='csv'
  )
  response <- httr::POST(redcapUrl, body = formData, encode = "form")
  result <- httr::content(response)
  if (httr::status_code(response) == 200) {
    return(tibble::tibble(result))
  } else {
    stop("Failed to download RedCap report. HTTP status code: ", httr::status_code(response))
  }
}
