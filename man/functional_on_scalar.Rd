\name{functional_on_scalar}
\alias{functional_on_scalar}
\title{Fit a Function-on-Scalar Regression}
\usage{
functional_on_scalar(y, x, yind, formula = y ~ x, ...)
}
\arguments{
  \item{y}{A numeric matrix with subjects in rows and time values in columns.}

  \item{x}{A numeric vector or data frame of scalar predictors.}

  \item{yind}{Numeric vector of time values matching the matrix columns.}

  \item{formula}{Formula for the scalar part of the model. Defaults to \code{y ~ x}.}

  \item{...}{Additional arguments passed to \code{refund::pffr()}.}
}
\value{
The fitted \code{pffr} model object.
}
\description{
Fits a function-on-scalar regression with \code{refund::pffr()} for cases where
a scalar response or contrast explains a functional weather series measured on a
common grid.
}




