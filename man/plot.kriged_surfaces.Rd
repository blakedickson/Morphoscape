\name{plot.kriged_surfaces}
\alias{plot.kriged_surfaces}

\title{
Plots Kriged surfaces of functional characteristics
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
\method{plot}{kriged_surfaces}(x, alpha = 0.5, pt.col = "black",
     interpolate = TRUE, contour = TRUE, ...)
}
\arguments{
  \item{x}{
a \code{kriged_surfaces} object; the output of a call to \code{\link{krige_surf}}.
}
  \item{alpha, pt.col}{
when a \code{new_data} component is present in \code{x}, the transperancy (\code{alpha}) and color (\code{pt.col}) of the points plotted for the new samples.
}
  \item{interpolate}{
\code{logical}; whether to smooth the plot by interpolating across pixels in the grid. Pased to \code{\link[ggplot2:geom_raster]{ggplot2::geom_raster}}.
}
  \item{contour}{
\code{logical}; whether to add contour lines to the plot to illustrate changes in the fitness landscape.
}
  \item{\dots}{
  ignored.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A \code{ggplot} object, which can be furtehr manipulated using \pkg{ggplot2} functionality.
}

\seealso{
\code{\link[ggplot2:ggplot]{ggplot2::ggplot}}, \code{\link[ggplot2:geom_raster]{ggplot2::geom_raster}}, and \code{\link[ggplot2:geom_contour]{ggplot2::geom_contour}} for the udnerlying plotting functions

\code{\link{krige_surf}} for generating the kriged surfaces. \code{\link{krige_new_data}} for adding a \code{new_data} component to an existing kriged surface before plotting.
}
\examples{
# See examples at help("krige_surf")
}