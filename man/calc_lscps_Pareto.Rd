\name{calc_lscp_Pareto}
\alias{calc_lscp_Pareto}

\title{Calculates Pareto optimality
%%  ~~function to do ... ~~
}
\description{Calculates Pareto optimality between two traits or landscapes Z values across morphospace.
}
\usage{
calc_lscp_Pareto(x,n1,n2)
}
\arguments{
  \item{x}{
    Oject of type 'by_Wprime' or 'kriged_surface'
 \item{n1}{
    Name or the first trait or group landscape for comparison
}
 \item{n2}{
    Name or the second trait or group landscape for comparison}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  An object of class \code{tibble} that contains the x,y grid of points across morphospace, the trait or landscape values Z being compared at each point and the associated Pareto optimality (Ri) values.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
 Robert J. Brocklehurst
}
\note{
%%  ~~further notes~~
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#########
data('warps')
data('turtles')

#########
warps_fnc <- as_fnc_df(warps, func.names = c("hydro", "curve", "mech", "fea"))
kr_surf <- krige_surf(warps_fnc, hull = FALSE, new_data = turtles)

KrSurfParOpt <- calc_lscp_Pareto(kr_surf,'hydro','mech')
head(KrSurfParOpt)

#########
weights <- generate_weights(n = 20, data = kr_surf)
all_landscapes <- calc_all_lscps(kr_surf, grid_weights = weights)
wprime_by_Group <- calcWprimeBy(all_landscapes, by = ~Ecology,method = "quantile")

LscpPaOpt <- calc_lscp_Pareto(wprime_by_Group,'M','S')
head(LscpPaOpt)
}
