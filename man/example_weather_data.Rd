\name{example_weather_data}
\alias{example_weather_data}
\title{Example Weather Data}
\usage{
example_weather_data(
  start = as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
  days = 30
)
}
\arguments{
  \item{start}{Start time for the series.}

  \item{days}{Number of days to generate.}
}
\value{
A data frame with hourly weather observations.
}
\description{
Generates a small synthetic hourly weather time series that can be used in
examples, tests, and package demonstrations.
}




