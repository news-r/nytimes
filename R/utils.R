BASE_URL <- "https://api.nytimes.com"

# get key
.get_key <- function() {
  key <- Sys.getenv("NYTIMES_API_KEY")
  assert_that(nchar(key) > 1, msg = "Missing key")
  return(key)
}