#' Semantic
#' 
#' The Semantic API complements the Articles API. With the Semantic API, 
#' you get access to the long list of people, places, organizations and 
#' other locations, entities and descriptors that make up the controlled 
#' vocabulary used as metadata by The New York Times (sometimes referred 
#' to as Times Tags and used for Times Topics pages.
#' 
#' @param q Precedes the search term string. Used in a Search Query. Except for \code{specific_concept_name}, 
#'  Search Query will take the required parameters listed above (\code{concept_type}, \code{concept_uri}, 
#'  \code{article_uri}) as an optional_parameter in addition to the query set to a \code{query_term}.
#' @param pages Number of pages of results to collect.
#' @param fields \code{all} or a vector of specific optional fields (see fields section).
#' @param concept The name of the concept, used for Constructing a Semantic API Request by Concept Type and Specific Concept Name, i.e.: \code{baseball}.
#' @param type The type of the concept, used for Constructing a Semantic API Request by Concept Type and Specific Concept Name. 
#' 
#' @section Fields:
#' Valid values for the \code{fields} argument and they explanation.
#' \itemize{
#'   \item{\code{all} - All values.}
#'   \item{\code{pages} - A list of topic pages associated with a specific concept.}
#'   \item{\code{ticker_symbol} - If this concept is a publicly traded company, this field contains the ticker symbol.}
#'   \item{\code{links} - A list of links from this concept to external data resources.}
#'   \item{\code{taxonomy} - For descriptor concepts, this field returns a list of taxonomic relations to other concepts.}
#'   \item{\code{combinations} - For descriptor concepts, this field returns a list of the specific meanings tis concept takes on when combined with other concepts.}
#'   \item{\code{geocodes} - For geographic concepts, the full GIS record from geonames.}
#'   \item{\code{article_list} - A list of up to 10 articles associated with this concept.}
#'   \item{\code{scope_notes} - Scope notes contains clarifications and meaning definitions that explicate the relationship between the concept and an article.}
#'   \item{\code{search_api_query} - Returns the request one would need to submit to the Article Search API to obtain a list of articles annotated with this concept.}
#' }
#' 
#' @examples
#' \dontrun{
#' nytimes_key("xXXxxXxXxXXx")
#' war <- ny_semantic_search("war")
#' war <- ny_semantic_concept("baseball", type = "nytd_des")
#' }
#' 
#' @name semantics
#' @export
ny_semantic_search <- function(q, pages = 1, fields = "all") {
  assert_that(!missing(q), msg = "Missing q")
  assert_that(pages > 0)

  if(is.infinite(pages)) pages <- 999999

  fields <- paste0(fields, collapse = ",")

  opts <- list(
    query = q,
    fields = fields,
    offset = 0,
    `api-key` = .get_key()
  )

  parsed_url <- parse_url(BASE_URL)
  parsed_url$path <- c("svc", "semantic", "v2", "concept", "search.json")

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
    } else {
      cat(crayon::blue(cli::symbol$info), page_content$num_results, "results available\n")
    }
    
    if(!isTRUE(page_content$has_more))
      break
  }

  pb$terminate()

  data <- content %>% 
    map("results") %>% 
    flatten()

  if(length(data) < page_content$num_results)
    cat(crayon::yellow(cli::symbol$warning), "More results available\n")

  data 
}

#' @rdname semantics
#' @export
ny_semantic_concept <- function(concept, q = NULL, type = c("nytd_geo", "nytd_per", "nytd_org", "nytd_des"), fields = "all"){
  assert_that(!missing(concept), msg = "Missing concept")
  fields <- paste0(fields, collapse = ",")
  concept <- paste0(concept, ".json")
  url <- parse_url(BASE_URL)
  url$path <- c("svc", "semantic", "v2", "concept", "name", match.arg(type), concept)
  url$query <- list(`api-key` = .get_key(), fields = fields, query = q)
  url <- build_url(url)
  
  response <- GET(url)
  stop_for_status(response)
  content <- content(response)
  cat(crayon::blue(cli::symbol$info), content$num_results, "results returned\n")
  
  return(content$results)
}