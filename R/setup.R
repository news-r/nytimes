#' Setup
#' 
#' Setup API key to be used for subsequent calls.
#' 
#' @param key Your API key, freely available at \url{https://developer.nytimes.com}.
#' 
#' @note You can specify \code{NYTIMES_API_KEY} as environment variable, likely in your \code{.Renviron} file.
#' 
#' @examples
#' \dontrun{
#' nytimes_key("xXXxxXxXxXXx")  
#' }
#' 
#' @import httr
#' @import assertthat
#' 
#' @export
nytimes_key <- function(key){
  assert_that(!missing(key), msg = "Missing key")
  Sys.setenv(NYTIMES_API_KEY = key)
}