<!-- README.md is generated from README.Rmd. Please edit that file -->



<!-- badges: start -->
[![Travis build status](https://travis-ci.org/news-r/nytimes.svg?branch=master)](https://travis-ci.org/news-r/nytimes)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/news-r/nytimes?branch=master&svg=true)](https://ci.appveyor.com/project/news-r/nytimes)
<!-- badges: end -->

# nytimes

The goal of `nytimes` is to integrate all of the [New York Times API](https://developer.nytimes.com) with R.

## Installation

``` r
#install.packages("remotes")
remotes::install_github("news-r/nytimes")
```

## APIs

- [x] [Archive](https://developer.nytimes.com/docs/archive-product/1/overview) 
- [x] [Article Search](https://developer.nytimes.com/docs/articlesearch-product/1/overview)
- [x] [Books](https://developer.nytimes.com/docs/books-product/1/overview)
- [x] [Geo](https://developer.nytimes.com/docs/geo-product/1/overview)
- [x] [Most Popular](https://developer.nytimes.com/docs/most-popular-product/1/overview)
- [x] [Movie Reviews](https://developer.nytimes.com/docs/movie-reviews-api/1/overview)
- [x] [Semantic](https://developer.nytimes.com/docs/semantic-api-product/1/overview)
- [x] [Times Tags](https://developer.nytimes.com/docs/timestags-product/1/overview)
- [x] [Times Wire](https://developer.nytimes.com/docs/timeswire-product/1/overview)
- [x] [Top Stories](https://developer.nytimes.com/docs/top-stories-product/1/overview)

## Setup

First, [create an account](https://developer.nytimes.com) to obtain an API key. Then either specify the aforementioned key using `nytimes_key` or specify it as environment variable (likely in your `.Renviron`) as `NYTIMES_API_KEY`.

```r
library(nytimes)

ny_key("xXxxX")
```

## Examples

The archive API.


```r
library(nytimes)

# get all articles from January first 2018
archive <- ny_archive(2018, 1)
#> ℹ 6903 results returned
```

The article search API.


```r
# get all articles on Obama that have been published in the last 3 days, get three pages of results
obama <- ny_search("Obama", since = Sys.Date() - 3, pages = 3)
#> ℹ 49 results available
#> 
  downloading [=======================================] 100%
```

The books API


```r
# get data on a random book
books <- ny_book_names()
#> ℹ 55 results returned
list <- ny_book_list(sample(books$list_name_encoded, 1))
#> ℹ 10 results returned
```

The most popular API


```r
# get most viewed articles in the last 7 days
viewed <- ny_popular_viewed(7)
#> ℹ 1716 results returned
```

The movie review API


```r
# get 2 pages of movie reviews on war
reviews <- ny_movie_search("war", pages = 2)
#> ⚠ More results available
```

The semantic API


```r
# get 2 pages of movie reviews on war
concepts <- ny_semantic_search("war")
#> ℹ 500 results available
#> ⚠ More results available
```

Times tags API


```r
ny_tags("Trump", max = 6)
#> [1] "Trump, Donald J (Per)"    "Trump Organization (Org)"
#> [3] "Trump, Ivanka (Per)"      "Trump, Melania (Per)"    
#> [5] "Trump, Donald J Jr (Per)" "Wallace, George C (Per)"
```

Top stories API

```r
business <- ny_stories("business")
```

Times wire API


```r
sections <- ny_wire_section_list()
wires <- ny_wire_source(sample(sections$section, 1))
#> ℹ 3298 results available
```
