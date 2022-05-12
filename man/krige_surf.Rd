\name{krige_surf}
\alias{krige_surf}
\alias{krige_new_data}
\alias{kriged_surfaces}

\title{
Interpolate functional characteristics over a grid
}
\description{
\code{krige_surf()} performs kriging (i.e., interpolation) of functional characteristics over a grid of morphological coordinates to create a kriged surface. Kriged values can also be produced for a new dataset given their coordinates in morphological space.
}
\usage{
krige_surf(fnc_df, grid = NULL, resample = 100,
           hull = TRUE, alpha = 1, new_data = NULL)
           
krige_new_data(x, new_data)
}
\arguments{
  \item{fnc_df}{
an \code{fnc_df} object; the output of a call to \code{\link{as_fnc_df}}, which contains coordinates in morphological space and values of functional characteristics for the warps used to create the kriged surface.
}
  \item{grid}{
a matrix or data frame contain the grid over which the surface is to be interpolated. Should be the output of a call to \code{\link{resample_grid}}. If \code{NULL}, the grid will be formed by calling \code{resample_grid()} on the inputs. In most cases \code{grid} should be left \code{NULL}.
}
  \item{resample}{
the number of points (or pixels) in the x and y dimensions over which to create the grid. Default is 100 for a kriged surface of 100x100=10,000 pixels. Passed to \code{\link{resample_grid}}. Ignored when \code{grid} is not \code{NULL}.
}
  \item{hull}{
whether to restrict kriging to an alpha hull to prevent extrapolation beyond the coordinates present in \code{fnc_df}. Passed to \code{\link{resample_grid}}, which uses \code{\link[alphahull:ahull]{alphahull::ahull}}. Default is \code{TRUE}. If \code{FALSE}, kriging will take place over a rectangular grid that spans the boundaries of the coordinates in \code{fnc_df}. Ignored when \code{grid} is not \code{NULL}.
}
  \item{alpha}{
the alpha value used to create the alpha hull. Passed to \code{\link{resample_grid}} and eventually to \code{\link[alphahull:ahull]{alphahull::ahull}}. Ignored when \code{grid} is not \code{NULL}.
}
  \item{new_data}{
a dataset of coordinates for a new sample; the values of the functional characteristics for this sample will be interpolated using the kriged surface.
}
  \item{x}{
a \code{"kriged_surfaces"} object; the output of a call to \code{krige_surf()}.
}
}
\details{
\code{krige_surf()} does stuff. EDIT

\code{krige_new_data()} adds a new data set to the supplied \code{kriged_surfaces} object and interpolates the values of the functional characteristics on the suppllied sample. This should only be used to add a new dataset to a \code{kriged_surfaces} object produced without \code{new_data} supplied or to replace the exist \code{new_data} component.
}
\value{
An object of class \code{kriged_surfaces} containing the following components:
\item{fnc_df}{the original data frame of functional characteristics passed to \code{fnc_df}.}
\item{autoKrige}{a named list of the kriged surfaces, one for each functional characteristic. Each surface is of class \code{autoKrige}, the output of the call to \code{\link[automap:autoKrige]{automap::autoKrige}}.}
\item{dataframes}{a list of two data frames, \code{grid} and \code{new_data}. \code{grid} contains the coordinates of the kriged surface grid (in the \code{x} and \code{y} columns) as well as the interpolated values of the functional characteristics. \code{new_data} contains the sample coordinates supplied to \code{new_data} as well as the interpolated values of the functional characteristics for each sample. This second component is absent if \code{new_data = NULL} in the call to \code{krige_surf()}.}

For \code{krige_new_data()}, the original \code{kriged_surfaces} object, with the \code{$dataframes$new_data} component containing the sample coordinates supplied to \code{new_data} as well as the interpolated values of the functional characteristics for each sample.
}

\seealso{
\code{\link{plot.kriged_surfaces}} for plotting the kriged surfaces.

\code{\link{as_fnc_df}} for creating the input dataset.

\code{\link{resample_grid}} for creating the grid over which the kriging occurs prior to using \code{krige_surf}.

\code{\link[automap:autoKrige]{automap::autoKrige}} for the function that does the kriging.
}
\examples{
data("warps")

warps_fnc <- as_fnc_df(warps)

grid <- resample_grid(warps)

kr_surf <- krige_surf(warps_fnc, grid = grid)
kr_surf

# Add new data
data("turtles")
kr_surf <- krige_new_data(kr_surf, new_data = turtles)
kr_surf
plot(kr_surf)

# Doing it all in one step:
\dontrun{
kr_surf <- krige_surf(warps_fnc, new_data = turtles)
kr_surf
}
}