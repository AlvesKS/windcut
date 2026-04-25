\name{screen_window_features}
\alias{screen_window_features}
\title{Screen Window-Derived Features Against a Disease Response}
\usage{
screen_window_features(
  data,
  response_col,
  feature_cols = NULL,
  method = c("spearman", "pearson"),
  adjust_method = "BH"
)
}
\arguments{
  \item{data}{A data frame that includes the response column and feature columns.}

  \item{response_col}{Name of the response column.}

  \item{feature_cols}{Optional character vector of feature columns to test. If \code{NULL}, all numeric columns except the response are screened.}

  \item{method}{Correlation method passed to \code{\link[stats:cor.test]{stats::cor.test()}}. One of \code{"spearman"} or \code{"pearson"}.}

  \item{adjust_method}{Method used by \code{\link[stats:p.adjust]{stats::p.adjust()}} to correct p-values.}
}
\value{
A data frame with feature names, parsed metric/window labels, correlation
estimates, raw p-values, adjusted p-values, and sample size.
}
\description{
Computes univariate associations between weather features produced by
\code{\link{window_pane}} and a disease response. This implements the screening
step commonly used after window-pane feature construction, where each
metric-window summary is related to the response using a correlation
coefficient and the resulting p-values are adjusted for multiple testing.
}




