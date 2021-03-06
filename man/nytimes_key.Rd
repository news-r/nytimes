% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setup.R
\name{nytimes_key}
\alias{nytimes_key}
\title{Setup}
\usage{
nytimes_key(key)
}
\arguments{
\item{key}{Your API key, freely available at \url{https://developer.nytimes.com}.}
}
\description{
Setup API key to be used for subsequent calls.
}
\note{
You can specify \code{NYTIMES_API_KEY} as environment variable, likely in your \code{.Renviron} file.
}
\examples{
\dontrun{
nytimes_key("xXXxxXxXxXXx")  
}

}
