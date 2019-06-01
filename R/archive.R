#' Archive
#' 
#' The Archive API returns an array of NYT articles for a given month, going back to 1851. 
#' Its response fields are the same as the Article Search API. The Archive API is very useful 
#' if you want to build your own database of NYT article metadata. You simply pass the API the 
#' year and month and it returns a JSON object with all articles for that month. 
#' The response size can be large (~20mb).
#' 
#' @param year Year, minimum \code{1851}.
#' @param month Month, integer between 1 and 12. 
#' 
#' @examples
#' \dontrun{
#' nytimes_key("xXXxxXxXxXXx")
#' archive <- ny_archive(2018, 1)
#' }
#' 
#' @export
ny_archive <- function(year, month) {
  max_year <- Sys.Date() %>% 
    format("%Y") %>% 
    as.integer()
  
  assert_that(year > 1851 && year < max_year)
  assert_that(month >= 1 && month <= 12)

  month <- paste0(month, ".json")
  url <- parse_url(BASE_URL)
  url$path <- c("svc", "archive", "v1", year, month)
  url$query <- list(`api-key` = .get_key())
  url <- build_url(url)
  
  response <- GET(url)
  stop_for_status(response)
  content <- content(response)
  return(content$response$docs)
}