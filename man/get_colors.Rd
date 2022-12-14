% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/palettes.R
\name{get_colors}
\alias{get_colors}
\title{Get names and hex codes from color palette}
\usage{
get_colors(..., palette = root_colors)
}
\arguments{
\item{...}{List of colors to return. If \code{NULL}, returns the entire set of options.}

\item{palette}{Named character vector of hex codes; defaults to \code{root_colors}.}
}
\description{
Get names and hex codes from color palette
}
\examples{
get_colors()
get_colors("cats")
get_colors("cats", "birds")
}
