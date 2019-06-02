#' Reviewers
#'
#' Get movie critics.
#' 
#' @param reviewer Reviewer name or \code{all} for all reviewers, \code{full-time} for full-time reviewers, or \code{part-time} for part-time reviewers.
#' 
#' @examples
#' \dontrun{
#' nytimes_key("xXXxxXxXxXXx")
#' reviewers <- ny_movie_reviewers("part-time")
#' }
#' 
#' @export
ny_movie_reviewers <- function(reviewer = "all"){
  reviewer <- paste0(reviewer, ".json")
  url <- parse_url(BASE_URL)
  url$path <- c("svc", "movies", "v2", "critics", reviewer)
  url$query <- list(`api-key` = .get_key())
  url <- build_url(url)
  
  response <- GET(url)
  stop_for_status(response)
  content <- content(response)
  cat(crayon::blue(cli::symbol$info), content$num_results, "results returned\n")
  
  return(content$results)
}

#' Reviews
#'
#' Get movie reviews.
#' 
#' @param type Filter by critiics, takes \code{all} or \code{picks}
#' 
#' @examples
#' \dontrun{
#' nytimes_key("xXXxxXxXxXXx")
#' reviewers <- ny_movie_reviewers("part-time")
#' }
#' 
#' @export
ny_movie_reviews <- function(type = c("all", "picks")){
  type <- paste0(match.arg(type), ".json")
  url <- parse_url(BASE_URL)
  url$path <- c("svc", "movies", "v2", "reviews", type)
  url$query <- list(`api-key` = .get_key())
  url <- build_url(url)
  
  response <- GET(url)
  stop_for_status(response)
  content <- content(response)
  cat(crayon::blue(cli::symbol$info), content$num_results, "results returned\n")
  
  return(content$results)
}

#' Search Movie
#'
#' Search for movie reviews.
#' 
#' @param q Search keyword (e.g. \code{lebowski}).
#' @param picks Set to \code{TRUE} to only show critics' picks. Otherwise shows both.
#' @param pages Number of pages of results to fetch, set to \code{Inf} to retrieve all.
#' @param order Field to order results by (e.g. \code{by-publication-date}).
#' @param reviewer Filter by reviewer byline (e.g. \code{Stephen Holden}).
#' @param opening_since,opening_until Date range as \code{Date} objects.
#' @param publication_since,publication_until Review publication date range as \code{Date} objects.
#' 
#' @examples
#' \dontrun{
#' nytimes_key("xXXxxXxXxXXx")
#' reviews <- ny_movie_search("wars", pages = 2)
#' }
#' 
#' @export
ny_movie_search <- function(q, picks = FALSE, pages = 1, opening_since = NULL, opening_until = NULL, order = NULL, 
  publication_since = NULL, publication_until = NULL, reviewer = NULL) {

  assert_that(!missing(q))
  assert_that(pages > 0)

  if(is.infinite(pages)) pages <- 999999

  opening_date <- .process_geo_daterange(opening_since, opening_until)
  publication_date <- .process_geo_daterange(publication_since, publication_until)
  if(isTRUE(picks))
    picks <- "Y"
  else
    picks <- NULL

  opts <- list(
    query = q,
    opening_date = opening_date,
    publication_date = publication_date,
    order = order, 
    reviewer = reviewer,
    picks = picks,
    `api-key` = .get_key()
  )

  parsed_url <- parse_url(BASE_URL)
  parsed_url$path <- c("svc", "movies", "v2", "reviews", "search.json")

  pb <- progress::progress_bar$new(
    format = "  downloading [:bar] :percent",
    total = pages - 1, clear = FALSE, width = 60
  )
  
  offset <- 0
  content <- list()
  for(p in 1:pages){
    opts$offset <- offset
    parsed_url$query <- opts
    url <- build_url(parsed_url)
    response <- GET(url)
    stop_for_status(response)
    page_content <- content(response)
    content <- append(content, list(page_content))

    # check if results left
    offset <- offset + 20
    if(p != 1) {
      pb$tick()
      Sys.sleep(6)
    }
    
    if(!isTRUE(page_content$has_more))
      break
  }

  pb$terminate()

  if(isTRUE(page_content$has_more))
    cat(crayon::yellow(cli::symbol$warning), "More results available\n")

  content %>% 
    map("results") %>% 
    flatten()
}