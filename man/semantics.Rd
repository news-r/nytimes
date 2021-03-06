% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/semantic.R
\name{semantics}
\alias{semantics}
\alias{ny_semantic_search}
\alias{ny_semantic_concept}
\title{Semantic}
\usage{
ny_semantic_search(q, pages = 1, fields = "all")

ny_semantic_concept(
  concept,
  q = NULL,
  type = c("nytd_geo", "nytd_per", "nytd_org", "nytd_des"),
  fields = "all"
)
}
\arguments{
\item{q}{Precedes the search term string. Used in a Search Query. Except for \code{specific_concept_name}, 
Search Query will take the required parameters listed above (\code{concept_type}, \code{concept_uri}, 
\code{article_uri}) as an optional_parameter in addition to the query set to a \code{query_term}.}

\item{pages}{Number of pages of results to collect.}

\item{fields}{\code{all} or a vector of specific optional fields (see fields section).}

\item{concept}{The name of the concept, used for Constructing a Semantic API Request by Concept Type and Specific Concept Name, i.e.: \code{baseball}.}

\item{type}{The type of the concept, used for Constructing a Semantic API Request by Concept Type and Specific Concept Name.}
}
\description{
The Semantic API complements the Articles API. With the Semantic API, 
you get access to the long list of people, places, organizations and 
other locations, entities and descriptors that make up the controlled 
vocabulary used as metadata by The New York Times (sometimes referred 
to as Times Tags and used for Times Topics pages.
}
\section{Fields}{

Valid values for the \code{fields} argument and they explanation.
\itemize{
  \item{\code{all} - All values.}
  \item{\code{pages} - A list of topic pages associated with a specific concept.}
  \item{\code{ticker_symbol} - If this concept is a publicly traded company, this field contains the ticker symbol.}
  \item{\code{links} - A list of links from this concept to external data resources.}
  \item{\code{taxonomy} - For descriptor concepts, this field returns a list of taxonomic relations to other concepts.}
  \item{\code{combinations} - For descriptor concepts, this field returns a list of the specific meanings tis concept takes on when combined with other concepts.}
  \item{\code{geocodes} - For geographic concepts, the full GIS record from geonames.}
  \item{\code{article_list} - A list of up to 10 articles associated with this concept.}
  \item{\code{scope_notes} - Scope notes contains clarifications and meaning definitions that explicate the relationship between the concept and an article.}
  \item{\code{search_api_query} - Returns the request one would need to submit to the Article Search API to obtain a list of articles annotated with this concept.}
}
}

\examples{
\dontrun{
nytimes_key("xXXxxXxXxXXx")
war <- ny_semantic_search("war")
war <- ny_semantic_concept("baseball", type = "nytd_des")
}

}
