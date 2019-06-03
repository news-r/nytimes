#' Book List
#'
#' Get Best Sellers list. If no date is provided returns the latest list.
#' 
#' @param list A \code{list_name_encoded} as returned by \code{ny_book_names}.
#' @param bestsellers_date The week-ending date for the sales reflected on \code{list-name}. 
#' Times best sellers lists are compiled using available book sale data. 
#' The \code{bestsellers_date} may be significantly earlier than published-date. 
#' For additional information, see the explanation at the bottom of any best-seller list page on NYTimes.com 
#' (example: Hardcover Fiction, published Dec. 5 but reflecting sales to Nov. 29).
#' @param published_date The date the best sellers list was published on NYTimes.com 
#' (different than bestsellers-date). Use "current" for latest list.
#' @param pages Number of pages of results to return.
#' 
#' @examples
#' \dontrun{
#' nytimes_key("xXXxxXxXxXXx")
#' books <- ny_book_names()
#' list <- ny_book_list(sample(books$list_name_encoded, 1))
#' }
#' 
#' @name book_list
#' @export
ny_book_list <- function(list, bestsellers_date = NULL, published_date = NULL, pages = 1){

  assert_that(!missing(list), msg = "Missing list")
  assert_that(pages > 0)
  
  parsed_url <- parse_url(BASE_URL)
  parsed_url$path <- c("svc", "books", "v3", "lists.json")
  opts <- list(
    list = list,
    `bestsellers-date` = .process_book_date(bestsellers_date),
    `published-date` = .process_book_date(published_date),
    `api-key` = .get_key()
  )

  p <- 0
  results <- 0
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
    content <- append(content, list(page_content$results))
    results <- results + page_content$num_results
    if(pages > 0) Sys.sleep(6)
    p <- p + 1
  }

  pb$terminate()

  cat(crayon::blue(cli::symbol$info), results, "results returned\n")
  
  flatten(content)
}

#' @rdname book_list
#' @export
ny_book_date_list <- function(list, published_date = NULL, pages = 1){

  assert_that(!missing(list), msg = "Missing list")
  
  parsed_url <- parse_url(BASE_URL)
  list <- paste0(list, ".json")
  parsed_url$path <- c("svc", "books", "v3", .process_book_date(published_date), "lists.json")
  opts <- list(`api-key` = .get_key())

  p <- 0
  results <- 0
  content <- list()
  pb <- progress::progress_bar$new(
    format = "  downloading [:bar] :percent",
    total = pages, clear = FALSE, width = 60
  )

  while(p < pages){
    if(pages > 1)
      pb$tick()
    opts$offset <- p * 20
    parsed_url$query <- opts
    url <- build_url(parsed_url)
    response <- GET(url)
    stop_for_status(response)
    page_content <- content(response)
    content <- append(content, list(page_content$results))
    results <- results + page_content$num_results
    if(p > 1) Sys.sleep(6)
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

#' Overview
#' 
#' Get top 5 books for all the Best Sellers lists for specified date.
#' 
#' @param published_date The best-seller list publication date. 
#' You do not have to specify the exact date the list was published. 
#' The service will search forward (into the future) for the closest publication date to the date you specify. 
#' For example, a request for \code{as.Date("2013-05-22")} will retrieve the list that was published on \code{05-26}.
#' 
#' @examples
#' \dontrun{
#' nytimes_key("xXXxxXxXxXXx")
#' list <- ny_book_overview(Sys.Date() - 365)
#' }
#' 
#' @export
ny_book_overview <- function(published_date = NULL){

  url <- parse_url(BASE_URL)
  url$path <- c("svc", "books", "v3", "lists", "overview.json")
  url$query <- list(
    `published-date` = .process_book_date(published_date),
    `api-key` = .get_key()
  )
  url <- build_url(url)
  
  response <- GET(url)
  stop_for_status(response)
  content <- content(response)
  cat(crayon::blue(cli::symbol$info), content$num_results, "results returned\n")
  
  content$results 
}

#' History & Reviews
#'
#' Get Best Sellers list history and book reviews.
#'
#' @param age_group The target age group for the best seller.
#' @param author The author of the best seller. 
#' The author field does not include additional contributors 
#' (see Data Structure for more details about the author and contributor fields). 
#' When searching the author field, you can specify any combination of first, middle and last names.
#' @param contributor The author of the best seller, as well as other contributors such as the illustrator 
#' (to search or sort by author name only, use author instead).
#' When searching, you can specify any combination of first, middle and last names of any of the contributors.
#' @param isbn nternational Standard Book Number, 10 or 13 digits. 
#' A best seller may have both 10-digit and 13-digit ISBNs, and may have multiple ISBNs of each type. 
#' To search on multiple ISBNs, separate the ISBNs with semicolons (example: 9780446579933;0061374229).
#' @param pages Number of pages of results to return.
#' @param price The publisher's list price of the best seller, including decimal point.
#' @param publisher The standardized name of the publisher.
#' @param title The title of the best seller. 
#' When searching, you can specify a portion of a title or a full title.
#'
#' @name history
#' @export
ny_book_history <- function(age_group = NULL, author = NULL, contributor = NULL, isbn = NULL, pages = 1, price = NULL, publisher = NULL, title = NULL){
  
  parsed_url <- parse_url(BASE_URL)
  parsed_url$path <- c("svc", "books", "v3", "lists", "best-sellers", "history.json")
  opts <- list(
    `age-group` = age_group,
    author = author,
    contributor = contributor,
    isbn = isbn,
    price = price,
    publisher = publisher,
    title = title,
    `api-key` = .get_key()
  )

  p <- 0
  results <- 0
  content <- list()
  pb <- progress::progress_bar$new(
    format = " downloading [:bar] :percent",
    total = pages, clear = FALSE, width = 60
  )

  while(p < pages){
    if(pages > 1)
      pb$tick()
    opts$offset <- p * 20
    parsed_url$query <- opts
    url <- build_url(parsed_url)
    response <- GET(url)
    stop_for_status(response)
    page_content <- content(response)
    content <- append(content, list(page_content$results))
    results <- results + page_content$num_results
    if(p > 1) Sys.sleep(6)
    p <- p + 1
  }

  cat(crayon::blue(cli::symbol$info), results, "results returned\n")
  
  return(content)
}

#' @rdname history
#' @export
ny_book_reviews <- function(isbn = NULL, title = NULL, author = NULL){

  url <- parse_url(BASE_URL)
  url$path <- c("svc", "books", "v3", "reviews.json")
  url$query <- list(
    isbn = isbn,
    title = title,
    author = author,
    `api-key` = .get_key()
  )
  url <- build_url(url)
  
  response <- GET(url)
  stop_for_status(response)
  content <- content(response)
  cat(crayon::blue(cli::symbol$info), content$num_results, "results returned\n")
  
  content$results 
}