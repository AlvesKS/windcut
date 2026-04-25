\name{make_windows}
\alias{make_windows}
\title{Create Candidate Relative-Time Windows}
\usage{
make_windows(
  min_offset = -30,
  max_offset = -1,
  width = 5,
  slide_by = 1,
  type = c("fixed", "variable"),
  reference_col = NULL
)
}
\arguments{
  \item{min_offset}{Minimum relative-time offset included in the grid.}

  \item{max_offset}{Maximum relative-time offset included in the grid.}

  \item{width}{Window duration in relative-time units. For example, with
\code{min_offset = -5}, \code{max_offset = 0}, and \code{width = 5}, the window is labeled
\code{window_m05_z00} and covers the five relative-time units ending at the
reference date. Use one value for a fixed-duration scan, such as
\code{width = 5}. Use several values for a variable-duration scan, such as
\code{width = 2:4} or \code{width = c(2, 4, 6)}.}

  \item{slide_by}{Number of relative-time units used when sliding the window
start. A value of \code{1} creates consecutive starts; a value of \code{2}
skips every other possible start.}

  \item{type}{Window-generation strategy. \code{"fixed"} creates one sliding
window width. \code{"variable"} creates windows from all values in \code{width}.
If \code{width} has more than one value, \code{type} is treated as
\code{"variable"}.}

  \item{reference_col}{Optional name of the reference timestamp column, such
as \code{"assessment_time"} or \code{"planting_time"}. This is stored as metadata on
the returned window grid and used by \code{\link[=window_pane]{window_pane()}} when its own
\code{reference_col} argument is \code{NULL}.}
}
\value{
A data frame with \code{relative_start}, \code{relative_end}, \code{width}, and \code{label}.
}
\description{
Generates windows relative to a reference date. Negative offsets are before
the reference date, zero is the reference date, and positive offsets are after
the reference date. Use fixed-width windows for the classic window-pane scan,
where one exposure duration slides through the relative-time range. Use
variable-width windows when you also want to scan alternative exposure
durations.
}
\details{
\code{make_windows()} does not place windows on calendar dates by itself. It
defines the relative-time grid. If \code{reference_col} is supplied, that column
name is stored with the grid so \code{\link[=window_pane]{window_pane()}} can
later place the windows relative to each row's site-specific reference date.
}
\examples{
make_windows(min_offset = -7, max_offset = -1, width = 5)

make_windows(min_offset = -21, max_offset = -1, width = 5, slide_by = 3)

make_windows(min_offset = -7, max_offset = -1, width = 5, reference_col = "planting_time")

make_windows(
  min_offset = -5,
  max_offset = 5,
  width = c(2, 4, 6),
  slide_by = 2
)
}




