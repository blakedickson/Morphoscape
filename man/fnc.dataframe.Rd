% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/landscape_functions.R
\name{fnc.dataframe}
\alias{fnc.dataframe}
\title{Coerces XY coordinate data and covariate data into a list, or optionally into
   a 3D array}
\usage{
fnc.dataframe(X, row.names, func.names = NULL, array = F, scale = T)
}
\arguments{
\item{X}{A matrix with the first two columns containing the XY coordinates, and subsequent columns contianing trait data}

\item{row.names}{row names}

\item{func.names}{An optional string containing the names for traits given
in z}

\item{array}{An optional logical argument to return a 3D array rather than a
list. By default is set to FALSE}

\item{scale}{An optional logical argument to normalize z covariates to the
same unit scale. Defaults to TRUE. Can be set to FALSE if the user wishes
to normalize manually}
}
\value{
Returns a list, or optionally an Array. Each sheet contains an x,y,z
    functional dataframe corresponding to covariates given in z,
    labelled by 'func.names'
}
\description{
Coerces XY coordinate data and covariate data into a list, or optionally into
   a 3D array
}
\examples{
X
}
