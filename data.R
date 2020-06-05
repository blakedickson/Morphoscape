#' Turtle humerus data
#'
#' A dataset containing the PC shape, functional traits, tree, and optimized 
#' weights for 40 turtle humeri
#'
#' @format Dataframes, vectors, chronos phylo tree, and Large bgPCA:
#' \describe{
#'   \item{pca}{between group PCA of 3D landmark data}
#'   \item{data}{functional trait dataframe containing PC coordinate and trait 
#'   measurements for 24 hypothetical morphospace shapes}
#'   \item{grp}{a vector of grouping factors for specimens}
#'   \item{grp.col}{a vector of grouping colours for convenient plotting}
#'   \item{chr.tree}{time calibrated phylogeny}
#'   \item{weights}{dataframe containing simplified optimal trait weights}
#'   \item{weights.df.list}{list containing expanded weighting results for each 
#'   grouping in grp}
#' }
#' @source Dickson, B. V & Pierce, S. E. Functional performance of turtle 
#' humerus shape across an ecological adaptive landscape. Evolution (N. Y). 
#' 73, 1265–1277 (2019).
"turtles"

