\name{resample_grid}
\alias{resample_grid}

\title{
Create a full grid from a set of coordinates
}
\description{
\code{resample_grid()} creates a rectangular grid around supplied coordinates by resampling evenly spaced points between the minimum and maximum values of each coordinate dimension. The grid can optionally be reduced to an alpha hull around the supplied coordinates.
}
\usage{
resample_grid(coords2D, resample = 100, padding = 1.2,
              hull = TRUE, alpha = 1, plot = FALSE)
}
\arguments{
  \item{coords2D}{
a 2-column matrix data frame of coordinates with the x-coordinates in the first column and the y-coordinates in the second column. The ranges of each column will be used to create the resampled grid.
}
  \item{resample}{
the number of points (or pixels) in the x and y dimensions over which to create the grid. Default is 100 for a kriged surface of 100x100=10,000 pixels.
}
  \item{padding}{
a number representing how much to expand the grid beyond the ranges of the x- and y-coordinates. For example, \code{padding = 1.2} (the default) expands the grid by 20\% of the coordinates' ranges in each direction. Must be a number greater than or equal to 1. Large numbers imply greater extrapolation, and whatever padding is added will be negated if \code{hull = TRUE}.
}
  \item{hull}{
whether to restrict the grid to an alpha hull using \code{\link[alphahull:ahull]{alphahull::ahull}}. Default is \code{TRUE}. 
}
  \item{alpha}{
when \code{hull = TRUE}, the alpha value used to create the hull. Passed to \code{\link[alphahull:ahull]{alphahull::ahull}}.
}
  \item{plot}{
when \code{hull = TRUE}, whether to plot the resulting hull overlayed over the original grid. Default is \code{TRUE}.
}
}

\value{
A data frame with two columns, \code{x} and \code{y}, containing the resampled coordinate grid. When \code{hull = TRUE}, any points not in the hull will be absent.
}

\seealso{
\code{\link{krige_surf}}, which uses \code{resample_grid} for kriging.

\code{\link[alphahull:ahull]{alphahull::ahull}} and \code{\link[alphahull:inahull]{alphahull::inahull}} for creating the hull.
}
\examples{
data("warps")

warps_fnc <- as_fnc_df(warps)

# Alpha hull with plot to see the hull
grid <- resample_grid(warps_fnc[c("x", "y")],
                      hull = TRUE, plot = TRUE)
str(grid)
}