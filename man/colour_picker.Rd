% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{colour_picker}
\alias{colour_picker}
\title{Get Colour Palette}
\usage{
colour_picker(
  type,
  unique_set_group,
  unique_set,
  colour_set_ordinal,
  colour_set_nominal
)
}
\arguments{
\item{type}{Character vector of variable types ("ordinal", "nominal",
"interval").}

\item{unique_set_group}{Character vector of unique values across the battery.}

\item{unique_set}{Character vector of unique values within the variable.}

\item{colour_set_ordinal, colour_set_nominal}{Character vector with hex
colours. Must be provided.}
}
\value{
Named character vector of hex colours for each element of unique_set.
}
\description{
Give two
}
