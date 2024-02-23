#' calc_lscp_Pareto
#'
#' Given a "by_Wprime" or "kriged_surfaces" object, this
#' function returns the grid of x, y values and adds a
#' unique grid cell identifier
#'
#' @param x A "by_Wprime" or "kriged_surfaces" object
#' @param n1 String, name of the first trait or group to be compared
#' @param n2 String, name of the second trait or group to be compared
#'
#' @return  A list that contains two elements: 
#' 		1) A tibble containing the x,y grid of points across morphospace, the 
#'		trait or landscape values Z being compared at each point and the 
#'		associated Pareto optimality (Ri) values.
#'		2) A character vector containing the names of the groups or traits that 
#'		had their Pareto optimality compared

calc_lscp_Pareto <- function(x,n1,n2){
  # Get the grid and landscape Z values from the
  # by_Wprime object
  POgrids <- get_grid(x)
  
  Ri <- calc_Ri(POgrids,n1,n2)
  POgrids$Ri <- Ri
  
  POgrids <- POgrids[,!(names(POgrids) %in% c('gridID'))]
  POgrids <- POgrids[,(names(POgrids) %in% c('x','y',n1,n2,'Ri'))]
  
  PO_out <- list(grid = POgrids,names = c(n1,n2))
  class(PO_out) <- "PO_List"
  
  return(PO_out)
}

#' Function for calculating Pareto optimality, using the formula of Deakin et al
#'
#' @param x The grid data from objects of class 'kriged_surface'
#'  or 'by_Wprime'
#' @param n1 Name of the first trait/group to be compared
#' @param n2 Name of the second trait/group to be compared
#'
#' @return Pareto optimality
#' @export
#'
#' @examples
calc_Ri <- function(x,n1,n2){
  
  #Standard optimality, get the rankings of each point on the grid
  Ro <- psel(x,high(get(n1)) * high(get(n2)),top = nrow(x))
  Ro <- Ro[order(Ro$gridID),]
  Ro <- Ro$.level
  
  #Inverse optimality, get the inverse rankings of each point on the grid
  Rs <- psel(x,low(get(n1)) * low(get(n2)),top = nrow(x))
  Rs <- Rs[order(Rs$gridID),]
  Rs <- Rs$.level
  
  # Calculate Pareto optimality using the Deakin et al formula
  Ri <- ((Rs-1)/(Ro+Rs-2))*1.01
  return(Ri)
}