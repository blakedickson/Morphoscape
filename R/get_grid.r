#' get_grid
#'
#' Given a "by_Wprime" or "kriged_surfaces" object, this
#' function returns the grid of x, y values and adds a
#' unique grid cell identifier
#'
#' @param x A "by_Wprime" or "kriged_surfaces" object
#'
#' @return The grid of x, y values and other data, with the
#' 		addition of a unique grid cell identifier, 'gridID', which
#' 		is needed to correctly reorder the data following the ranking
#' 		functions - see below.

get_grid <- function(x){
  # Get grid from a by_Wprime object
  if (inherits(x, "by_Wprime")){
    y <- x$grp_Wprimes[[1]]$Wprime$Wprime$grid
    y <- y[,!(names(y) %in% c('Z'))]
    y$gridID = 1:nrow(y)
    
    LS <- as.character(levels(x$by))
    LSZ <- sapply(LS,function(i) {
      zcol <- x$grp_Wprimes[[i]]$Wprime$Wprime$grid
      zcol <- zcol$Z
      return(zcol)
    })
    y <- cbind.data.frame(y,LSZ)
    
    return(y)
  }
  
  # Get grid from a kriged_surfaces object
  else if (inherits(x, "kriged_surfaces")){
    y <- x$dataframes$grid
    y$gridID = 1:nrow(y)
    return(y)
  }
  
  else {print('Data is not correct format')}
}

#' sp_vals_from_grid
#'
#' Function to get the heights on a landscape for a set
#' of species points.
#' For a given set of species coordinates in morphospace,
#' this function finds the closest point on the landscape
#' grid, and assigns the values associated to that grid
#' point to that species.
#'
#' @param x A 'by_Wprime' or 'PO_List' object - contains
#' the functional adaptive landscapes or Pareto landscapes
#' you want to extract heights from.
#' @param y A dataframe containing species information morphospace. The coordinates
#' columns must be labelled 'x' and 'y'.
#'
#' @return A dataframe containing the species information from y,
#' with additional columns extracted from the landscape data
#' 
sp_vals_from_grid <- function(x,y){
  if (inherits(x, c("by_Wprime", "kriged_surfaces"))){
    g <- get_grid(x)
  } else if(inherits(x, 'PO_List')){
    g <- x$grid
  } else {g <- x}
  grd2 <- g[,c('x','y')]
  dfd2 <- y[,c('x','y')]
  clsst.indx <- apply(dfd2,1,function(x){
    distance <- sqrt(rowSums((grd2-do.call(rbind,replicate(nrow(grd2),x,simplify = FALSE)))**2))
    nearest <- which.min(distance)
  })
  if(inherits(x, c("by_Wprime"))){
    grd_sub <- g[clsst.indx,levels(x$by)]
    grd_sub <- as.data.frame(grd_sub)
  }
  if(inherits(x, c("kriged_surfaces"))){
    grd_sub <- g[clsst.indx,names(x$autoKrige)]
    grd_sub <- as.data.frame(grd_sub)
  }
  if(inherits(x, 'PO_List')){
    grd_sub <- g[clsst.indx,'Ri']
    grd_sub <- as.data.frame(grd_sub)
    colnames(grd_sub)[1] <- paste0(x$names,collapse = '-')
  }
  result <- cbind.data.frame(y,grd_sub)
  return(result)
}
