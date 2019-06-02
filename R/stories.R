#' Top Stories
#' 
#' The Top Stories API returns an array of articles currently on the specified section.
#' 
#' @param section A valid section, one of \code{arts}, \code{automobiles}, \code{books}, 
#'  \code{business}, \code{fashion}, \code{food}, \code{health}, \code{home}, \code{insider}, 
#'  \code{magazine}, \code{movies}, \code{national}, \code{nyregion}, \code{obituaries}, 
#'  \code{opinion}, \code{politics}, \code{realestate}, \code{science}, \code{sports}, 
#'  \code{sundayreview}, \code{technology}, \code{theater}, \code{tmagazine}, \code{travel}, 
#'  \code{upshot}, \code{world}.
#' 
#' @examples
#' \dontrun{
#' nytimes_key("xXXxxXxXxXXx")
#' bus <- ny_stories("business")
#' }
#' 
#' @export
ny_stories <- function(section = "technology"){
  assert_that(section %in% VALID_SECTIONS, msg = "Wrong section")
  section <- paste0(section, ".json")
  url <- parse_url(BASE_URL)
  url$path <- c("svc", "topstories", "v2", section)
  url$query <- list(`api-key` = .get_key())
  url <- build_url(url)
  
  response <- GET(url)
  stop_for_status(response)
  content <- content(response)
}

VALID_SECTIONS <- c("arts", "automobiles", "books", "business", "fashion", "food", "health", 
  "home", "insider", "magazine", "movies", "national", "nyregion", "obituaries", "opinion", 
  "politics", "realestate", "science", "sports", "sundayreview", "technology", "theater", 
  "tmagazine", "travel", "upshot", "world")