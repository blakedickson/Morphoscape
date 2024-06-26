% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_lscp_Par.r
\name{plot_ggPareto}
\alias{plot_ggPareto}
\title{plot_ggPareto}
\usage{
plot_ggPareto(x, n1 = NULL, n2 = NULL)
}
\arguments{
\item{x}{PO_List object, result of calc_lscp_Pareto. Alternatively, a dataframe or tibble containing trait values}

\item{n1}{First variable to plot}

\item{n2}{Second variable to plot}
}
\value{
A ggplot object
}
\description{
Helper function - plots Pareto optimality Ri across the grid.
Contours are color coded according Ri score. Additionally,
the Pareto front is drawn using geom_smooth on the subset of
points whose Ri == maximum Ri
}
