#' Geo
#' 
#' The Geographic API extends the Semantic API, using a linked data approach to enhance 
#' location concepts used in The New York Times' controlled vocabulary and data resources 
#' which combine them with the GeoNames database, an authoritative and free to use database 
#' of global geographical places, names and features.
#' 
#' @param sw,ne Southwest and Northeast, used in combination with \code{longitude} and \code{latitude} to form a bounding box.
#' @param name A displayable name for the specified place.
#' @param longitude,latitude Coordinates of the specified place.
#' @param elevation The elevation of the specified place, in meters.
#' @param filter Filters search results based on the facets provided. For more information on the values you can filter on, see \code{facets}.
#' @param since,until Date range as \code{Date} objects.
#' @param facets When facets is set to \code{TRUE}, a count of all facets will be included in the response.
#' @param sort Sorts your results on the fields specified. 
#' @param pages Number of pages of results to retrieve
#' 
#' @export
ny_geo <- function(name = NULL, longitude = NULL, latitude = NULL, ne = NULL, sw = NULL, elevation = NULL, 
  filter = NULL, since = NULL, until = NULL, facets = FALSE, sort = NULL, pages = 1){
  
  date_range <- .process_geo_daterange(since, until)

  parsed_url <- parse_url(BASE_URL)
  parsed_url$path <- c("svc", "semantic", "v2", "geocodes", "query.json")
  opts <- list(
    sw = sw, 
    name = name, 
    longitude = longitude, 
    latitude = latitude, 
    elevation = elevation, 
    filter = filter, 
    date_range = date_range, 
    facets = as.integer(facets), 
    sort = sort,
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