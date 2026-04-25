\name{functional_group_summary}
\alias{functional_group_summary}
\title{Summarise Functional Curves by Group}
\usage{
functional_group_summary(
  data,
  id_col,
  time_col,
  value_col,
  group_col,
  time_grid = NULL,
  method = c("none", "lowess", "spline"),
  smooth_args = list()
)
}
\arguments{
  \item{data}{A data frame in long format.}

  \item{id_col}{Subject identifier column.}

  \item{time_col}{Time column, such as relative day or days after planting.}

  \item{value_col}{Measured series column.}

  \item{group_col}{Grouping column, such as disease status.}

  \item{time_grid}{Optional grid where the summary curves are evaluated. If
  \code{NULL}, the sorted observed time values are used. Supplying a denser
  grid is useful when plotting smoothed curves as continuous functions.}

  \item{method}{Smoothing method used on the group mean curves. One of
  \code{"none"}, \code{"lowess"}, or \code{"spline"}.}

  \item{smooth_args}{Named list of extra arguments passed to the smoothing
  method. For \code{"lowess"}, a common choice is \code{list(f = 0.2)}. For
  \code{"spline"}, typical choices include \code{list(df = 12)} or
  \code{list(spar = 0.6)}.}
}
\value{
A list with grouped summary tables and, when two groups are present,
pointwise difference tables. The returned object includes \code{summary},
\code{difference}, \code{raw_summary}, \code{raw_difference}, \code{method},
and \code{smooth_args}.
}
\description{
Computes mean functional curves by group and, when exactly two groups are
present, also returns the pointwise difference between those curves.
}




