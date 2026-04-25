\name{window_pane_demo_data}
\alias{window_pane_demo_data}
\docType{data}
\title{Demo Data for Window-Pane Workflows}
\format{
A list with three components:
\describe{
\item{weather}{Daily weather data for multiple simulated sites, all with the
same time-series length and at least 30 days before planting.}
\item{weather_hourly}{Hourly weather data used to create \code{weather}.}
\item{assessments}{One disease assessment per site, including
\code{assessment_time}, \code{planting_time}, and \code{disease_intensity}.}
}
}
\description{
A bundled dataset for the getting-started, window-pane, and feature-screening
tutorials.
}
\details{
The data were generated with \code{\link{simulate_weather_series}} and
\code{\link{simulate_assessment_data}} and then saved as package data so the
main workflow tutorials can begin directly with analysis.
}
\keyword{datasets}




