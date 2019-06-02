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

.process_geo_daterange <- function(x, y){
  dr <- NULL
  if(!is.null(x) && !is.null(y))
    dr <- paste0(format(x, "%Y-%m-%d"), ":", format(y, "%Y-%m-%d"))

  if((!is.null(x) && is.null(y)) || (is.null(x) && !is.null(y)))
    warning("Must provide `since` AND `until`, not either; ignoring these arguments", call. = FALSE)
  return(dr)
}