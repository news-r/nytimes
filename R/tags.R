#' Tags
#' 
#' With the TimesTags API, you can mine the riches of the New York Times tag set. 
#' The TimesTags service matches your query to the controlled vocabularies that 
#' fuel NYTimes.com metadata. You supply a string of characters, and the service 
#' returns a ranked list of suggested terms.
#' 
#' @param q Your search query.
#' @param filter If you do not specify a value for filter (see the Optional Parameters), 
#' your query will be matched to tags in all four Times dictionaries: subject (\code{Des}), 
#' geographic location (\code{Geo}), organization (\code{Org}) and person (\code{Per}). 
#' You can specify moret than one in  a vector.
#' @param max Maximum number of results to return.
#' 
#' @examples
#' \dontrun{
#' nytimes_key("xXXxxXxXxXXx")
#' trump <- ny_tags("Trump")
#' }
#' 
#' @export
ny_tags <- function(q, filter = NULL, max = 25) {
  assert_that(!missing(q), msg = "Missing q")
  url <- parse_url(BASE_URL)
  url$path <- c("svc", "suggest", "v1", "timestags")
  url$query <- list(`api-key` = .get_key(), query = q, filter = filter, max = max)
  url <- build_url(url)
  
  response <- GET(url)
  stop_for_status(response)
  content <- content(response)
  
  jsonlite::fromJSON(content)[[2]]
}