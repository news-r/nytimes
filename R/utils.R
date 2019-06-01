BASE_URL <- "https://api.nytimes.com"

# get key
.get_key <- function() {
  key <- Sys.getenv("NYTIMES_API_KEY")
  assert_that(nchar(key) > 1, msg = "Missing key")
  return(key)
}

.process_search_date <- function(x){
  if(!is.null(x))
    x <- format(x, "%Y%m%d")
  return(x)
}

.process_book_date <- function(x){
  if(!is.null(x))
    x <- format(x, "%Y-%m-%d")
  return(x)
}