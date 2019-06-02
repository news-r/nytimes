#' Popular
#' 
#' Provides services for getting the most popular articles on NYTimes.com based on emails, shares, or views.
#' 
#' @param period Time period: 1, 7, or 30 days.
#' @param type Share type: \code{email}, \code{facebook}, or \code{twitter}.
#' 
#' @section Functions:
#' \itemize{
#'   \item{\code{ny_popular_emailed} Returns an array of the most emailed articles on NYTimes.com for specified period of time.}
#'   \item{\code{ny_popular_shared} Returns an array of the most shared articles on NYTimes.com for specified period of time.}
#'   \item{\code{ny_popular_shared_type} Returns an array of the most shared articles by share type on NYTimes.com for specified period of time.}
#'   \item{\code{ny_popular_viewed} Returns an array of the most viewed articles on NYTimes.com for specified period of time.}
#' }
#' 
#' @examples
#' \dontrun{
#' nytimes_key("xXXxxXxXxXXx")
#' emailed <- ny_popular_emailed(2)
#' }
#' 
#' @name popular
#' @export
ny_popular_emailed <- function(period = 30) {
  assert_that(period %in% c(1, 7, 30), msg = "`period` argument accepts 1, 7, or 30")
  period <- paste0(period, ".json")
  url <- parse_url(BASE_URL)
  url$path <- c("svc", "mostpopular", "v2", "emailed", period)
  url$query <- list(`api-key` = .get_key())
  url <- build_url(url)
  
  response <- GET(url)
  stop_for_status(response)
  content <- content(response)
  cat(crayon::blue(cli::symbol$info), content$num_results, "results returned\n")
  
  return(content$results)
}

#' @rdname popular
#' @export
ny_popular_shared <- function(period = 30) {
  assert_that(period %in% c(1, 7, 30), msg = "`period` argument accepts 1, 7, or 30")
  period <- paste0(period, ".json")
  url <- parse_url(BASE_URL)
  url$path <- c("svc", "mostpopular", "v2", "shared", period)
  url$query <- list(`api-key` = .get_key())
  url <- build_url(url)
  
  response <- GET(url)
  stop_for_status(response)
  content <- content(response)
  cat(crayon::blue(cli::symbol$info), content$num_results, "results returned\n")
  
  return(content$results)
}

#' @rdname popular
#' @export
ny_popular_shared_type <- function(period = 30, type = c("email", "facebook", "twitter")) {
  assert_that(period %in% c(1, 7, 30), msg = "`period` argument accepts 1, 7, or 30")
  type <- paste0(match.arg(type), ".json")
  url <- parse_url(BASE_URL)
  url$path <- c("svc", "mostpopular", "v2", "shared", period, type)
  url$query <- list(`api-key` = .get_key())
  url <- build_url(url)
  
  response <- GET(url)
  stop_for_status(response)
  content <- content(response)
  cat(crayon::blue(cli::symbol$info), content$num_results, "results returned\n")
  
  return(content$results)
}

#' @rdname popular
#' @export
ny_popular_viewed <- function(period = 30) {
  assert_that(period %in% c(1, 7, 30), msg = "`period` argument accepts 1, 7, or 30")
  period <- paste0(period, ".json")
  url <- parse_url(BASE_URL)
  url$path <- c("svc", "mostpopular", "v2", "viewed", period)
  url$query <- list(`api-key` = .get_key())
  url <- build_url(url)
  
  response <- GET(url)
  stop_for_status(response)
  content <- content(response)
  cat(crayon::blue(cli::symbol$info), content$num_results, "results returned\n")
  
  return(content$results)
}