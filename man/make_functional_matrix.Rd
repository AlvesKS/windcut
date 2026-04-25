\name{make_functional_matrix}
\alias{make_functional_matrix}
\title{Create a Functional Matrix from Long Data}
\usage{
make_functional_matrix(
  data,
  id_col,
  time_col,
  value_col,
  time_grid = NULL,
  fill = NA_real_
)
}
\arguments{
  \item{data}{A data frame in long format.}

  \item{id_col}{Subject identifier column.}

  \item{time_col}{Time column, such as relative day or days after planting.}

  \item{value_col}{Measured series column.}

  \item{time_grid}{Optional vector defining the common time grid. If \code{NULL}, the sorted unique values in \code{time_col} are used.}

  \item{fill}{Value used for missing combinations.}
}
\value{
A list with \code{y}, \code{time}, and \code{id}.
}
\description{
Converts long-format repeated measurements into a common-grid matrix suitable
for functional analyses. Each row corresponds to one subject and each column to
one time value, such as days after planting.
}




