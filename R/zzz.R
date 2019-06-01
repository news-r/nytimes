.onAttach <- function(libname = find.package("nytimes"), pkgname = "nytimes") {

  key <- Sys.getenv("NYTIMES_API_KEY")

  msg <- "No API key found, see `nytimes_key`"
  if(nchar(key) > 1) msg <- "API key loaded!"

  packageStartupMessage(msg)

}