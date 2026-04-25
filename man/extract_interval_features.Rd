\name{extract_interval_features}
\alias{extract_interval_features}
\title{Extract Features from Functional Intervals}
\usage{
extract_interval_features(
  data,
  id_col,
  time_col,
  value_col,
  intervals,
  prefix = NULL,
  statistics = "mean"
)
}
\arguments{
  \item{data}{A data frame in long format.}

  \item{id_col}{Subject identifier column.}

  \item{time_col}{Time column, such as relative day or days after planting.}

  \item{value_col}{Measured series column.}

  \item{intervals}{A data frame with \code{start} and \code{end} columns.}

  \item{prefix}{Prefix used when naming the resulting features.}

  \item{statistics}{Interval statistics to compute. Use function names such as
\code{"mean"}, \code{"sd"}, and \code{"IQR"}, or a named list of custom functions.}
}
\value{
A wide data frame with one row per subject and one column per interval-derived
feature.
}
\description{
Uses one or more time intervals to compute subject-level summaries from a
long-format weather series. This is useful after interval-wise testing has
identified periods of functional divergence.
}




