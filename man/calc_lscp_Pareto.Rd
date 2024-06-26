% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_lscp_Par.r
\name{calc_lscp_Pareto}
\alias{calc_lscp_Pareto}
\title{calc_lscp_Pareto}
\usage{
calc_lscp_Pareto(x, n1, n2)
}
\arguments{
\item{x}{A "by_Wprime" or "kriged_surfaces" object}

\item{n1}{String, name of the first trait or group to be compared}

\item{n2}{String, name of the second trait or group to be compared}
}
\value{
A list that contains two elements: 
		1) A tibble containing the x,y grid of points across morphospace, the 
	trait or landscape values Z being compared at each point and the 
	associated Pareto optimality (Ri) values.
	2) A character vector containing the names of the groups or traits that 
	had their Pareto optimality compared
}
\description{
Given a "by_Wprime" or "kriged_surfaces" object, this
function returns the grid of x, y values and adds a
unique grid cell identifier
}
