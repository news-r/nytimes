#' Wire
#' 
#' With the Times Newswire API, you can get links and metadata for Times' 
#' articles as soon as they are published on NYTimes.com. The Times Newswire 
#' API provides an up-to-the-minute stream of published articles. You can 
#' filter results by source (all, nyt, or iht) and section (arts, business, ...).
#' 
#' @param url The complete URL of a specific news item.
#' @param source Name of the source; \code{all}, New York Times \code{nyt}, International New York Times \code{iht}.
#' @param section Name of section as returned by \code{ny_wire_section_list}.
#' @param pages Number of pages of results to return.
#' @param period Limits the set of items by time published, integer in number of hours.
#' 
#' @section Functions:
#' \itemize{
#' \item{\code{ny_wire_content} - Get content based on URL.} 
#' \item{\code{ny_wire_section_list} - Returns list of sections, useful for other calls.} 
#' \item{\code{ny_wire_source} - Returns list of wires by source and section.}
#' \item{\code{ny_wire_period} - Returns list of wires by source limited by period.} 
#' }
#' 
#' @examples
#' \dontrun{
#' nytimes_key("xXXxxXxXxXXx")
#' sections <- ny_wire_section_list()
#' wires <- ny_wire_source(sample(sections$section, 1))
#' }
#' 
#' @name wire
#' @export
ny_wire_content <- function(url) {
  assert_that(!missing(url), msg = "Missing url")
  uri <- url
  url <- parse_url(BASE_URL)
  url$path <- c("svc", "news", "v3", "content.json")
  url$query <- list(`api-key` = .get_key(), url = uri)
  url <- build_url(url)
  
  response <- GET(url)
  stop_for_status(response)
  content(response)
}

#' @rdname wire
#' @export
ny_wire_section_list <- function() {
  url <- parse_url(BASE_URL)
  url$path <- c("svc", "news", "v3", "content", "section-list.json")
  url$query <- list(`api-key` = .get_key())
  url <- build_url(url)
  
  response <- GET(url)
  stop_for_status(response)
  content <- content(response)
  map_dfr(content$results, tibble::as_tibble)
}

#' @rdname wire
#' @export
ny_wire_source <- function(section, pages = 1, source = c("all", "nyt", "iht")) {
  assert_that(!missing(section), msg = "Missing section")
  assert_that(pages > 0)

  source <- match.arg(source)

  section <- paste0(section, ".json")
  parsed_url <- parse_url(BASE_URL)
  parsed_url$path <- c("svc", "news", "v3", "content", source, section)
  opts <- list(
    limit = 20,
    offset = 0,
    `api-key` = .get_key()
  )

  p <- 0
  content <- list()
  pb <- progress::progress_bar$new(
    format = "  downloading [:bar] :percent",
    total = pages, clear = FALSE, width = 60
  )

  while(p < pages){
    pb$tick()
    opts$offset <- p * 20
    parsed_url$query <- opts
    url <- build_url(parsed_url)
    response <- GET(url)
    stop_for_status(response)
    page_content <- content(response)
    if(p == 0)
      cat(crayon::blue(cli::symbol$info), page_content$num_results, "results available\n")
    content <- append(content, list(page_content$results))
    if(pages > 0) Sys.sleep(12)
    p <- p + 1
    if(length(content) * 20  >= page_content$num_results)
      break
  }
  
  flatten(content)
}

#' @rdname wire
#' @export
ny_wire_period <- function(section, period = 12, pages = 1, source = c("all", "nyt", "iht")) {
  assert_that(!missing(section), msg = "Missing section")
  assert_that(pages > 0)

  source <- match.arg(source)

  period <- paste0(period, ".json")
  parsed_url <- parse_url(BASE_URL)
  parsed_url$path <- c("svc", "news", "v3", "content", source, section, period)
  opts <- list(
    limit = 20,
    offset = 0,
    `api-key` = .get_key()
  )

  p <- 0
  content <- list()
  pb <- progress::progress_bar$new(
    format = "  downloading [:bar] :percent",
    total = pages, clear = FALSE, width = 60
  )

  while(p < pages){
    pb$tick()
    opts$offset <- p * 20
    parsed_url$query <- opts
    url <- build_url(parsed_url)
    response <- GET(url)
    stop_for_status(response)
    page_content <- content(response)
    if(p == 0)
      cat(crayon::blue(cli::symbol$info), page_content$num_results, "results available\n")
    content <- append(content, list(page_content$results))
    if(pages > 0) Sys.sleep(12)
    p <- p + 1
    if(length(content) * 20  >= page_content$num_results)
      break
  }
  
  flatten(content)
}
