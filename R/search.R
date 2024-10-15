#' Search
#' 
#' Search for NYT articles by keywords, filters and facets.
#' 
#' @param q Search query.
#' @param since,until Begin and start \code{Date} objects.
#' @param pages Number of pages of results to return. Set to infinite (\code{Inf}) to retrieve all pages (\emph{not recommended}).
#' @param sort Sort order \code{newest}, \code{oldest}, \code{relevance}.
#' @param facets Whether to show facet counts, boolean.
#' @param facet_fields The following values are allowed: \code{day_of_week}, \code{document_type}, \code{ingredients}, 
#'  \code{news_desk}, \code{pub_month}, \code{pub_year}, \code{section_name}, \code{source}, \code{subsection_name}, \code{type_of_material}.
#' @param facet_filter Have facet counts use filters, boolean.
#' @param fl List of fields to return.
#' @param fq Query filter.
#' 
#' @examples
#' \dontrun{
#' nytimes_key("xXXxxXxXxXXx")
#' trump <- ny_search("Trump", since = Sys.Date() - 3)
#' }
#' 
#' @export
ny_search <- function(q, since = NULL, until = NULL, pages = 1, sort = c("newest", "oldest", "relevance"), 
  facets = FALSE, facet_fields = NULL, facet_filter = NULL, fl = NULL, fq = NULL){

  assert_that(!missing(q))
  assert_that(pages > 0)

  if(is.infinite(pages)) pages <- 999999

  opts <- list(
    q = q,
    begin_date = .process_search_date(since), 
    end_date = .process_search_date(until), 
    sort = match.arg(sort),
    facets = facets, 
    facet_fields = facet_fields, 
    facet_filter = facet_filter, 
    fl = fl, 
    fq = fq,
    `api-key` = .get_key()
  )

  parsed_url <- parse_url(BASE_URL)
  parsed_url$path <- c("svc", "search", "v2", "articlesearch.json")

  pb <- progress::progress_bar$new(
    format = "  downloading [:bar] :percent",
    total = pages - 1, clear = FALSE, width = 60
  )
  
  content <- list()
  for(p in 0:pages){
    opts$page <- p
    parsed_url$query <- opts
    url <- build_url(parsed_url)
    response <- GET(url)
    stop_for_status(response)
    page_content <- content(response)
    content <- append(content, list(page_content))

    # check if results left
    hits <- page_content$response$meta$hits
    offset <- page_content$response$meta$offset
    if(p == 0){
      cat(crayon::blue(cli::symbol$info), hits, "results available\n")
    } else {
      pb$tick()
      Sys.sleep(12)
    }
    
    if(offset >= hits)
      break
  }

  pb$terminate()

  content %>% 
    map("response") %>% 
    transpose()
}
