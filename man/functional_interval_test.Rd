\name{functional_interval_test}
\alias{functional_interval_test}
\title{Run an Interval-Wise Functional Test}
\usage{
functional_interval_test(
  group0,
  group1,
  n_permutations = 1000,
  alpha = 0.05,
  time = NULL,
  quiet = TRUE
)
}
\arguments{
  \item{group0}{Numeric matrix for the reference group.}

  \item{group1}{Numeric matrix for the comparison group.}

  \item{n_permutations}{Number of permutations used by the interval-wise test.}

  \item{alpha}{Significance level used when extracting intervals.}

  \item{time}{Numeric vector of time values corresponding to matrix columns.}

  \item{quiet}{Logical; if \code{TRUE}, suppresses routine console output emitted by
  \code{fdatest::ITP2bspline()}.}
}
\value{
A list with the raw test object, a table of corrected p-values by
time value, the selected significance level, the significant time
values, and a table of collapsed significant intervals.
}
\description{
Runs \code{fdatest::ITP2bspline()} to compare two groups of functional
observations evaluated on the same grid.
}




