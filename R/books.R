#' Book List
#'
#' Get Best Sellers list. If no date is provided returns the latest list.
#' 
#' @name book_list
#' @export
ny_book_list <- function(list, bestsellers_date = NULL, published_date = NULL, pages = 1){

  assert_that(!missing(list), msg = "Missing list")
  
  parsed_url <- parse_url(BASE_URL)
  parsed_url$path <- c("svc", "books", "v3", "lists.json")
  opts <- list(
    list = list,
    `bestsellers-date` = bestsellers_date,
    `published-date` = published_date,
    `api-key` = .get_key()
  )

  p <- 0
  results <- 0
  content <- list()
  while(p < pages){
    opts$offset <- (pages - 1) * 20
    parsed_url$query <- opts
    url <- build_url(parsed_url)
    response <- GET(url)
    stop_for_status(response)
    page_content <- content(response)
    content <- append(content, list(page_content$results))
    results <- results <- page_content$num_results
    p <- p + 1
  }

  cat(crayon::blue(cli::symbol$info), results, "results returned\n")
  
  return(content)
}

#' @rdname book_list
#' @export
ny_book_names <- function(){
  url <- parse_url(BASE_URL)
  url$path <- c("svc", "books", "v3", "lists", "names.json")
  url$query <- list(`api-key` = .get_key())
  url <- build_url(url)
  
  response <- GET(url)
  stop_for_status(response)
  content <- content(response)
  cat(crayon::blue(cli::symbol$info), content$num_results, "results returned\n")
  
  content$results %>% 
    map_dfr(tibble::as_tibble)
}
