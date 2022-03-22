#' Plot a heatmap of functional data.

#'
#' @param x,y,z x can be an xyz dataframe (such as from fnc.df),
#'   or the user can provide separate xyz vectors
#' @param ... parameters to pass onto image()
#'
#' @export
#' @examples X
plotImage<- function(x, y=NULL, z=NULL, ...){
  
  if(isTRUE(ncol(x)==3)){
    X <- x
  }else{
    X <- data.frame(x,y,z)
  }
  
  X <- X[order(X[,1],X[,2]) ,]
  
  XY <- cbind(unique(X[,1]), unique(sort(X[,2])) )
  Z <- matrix(X[,3], nrow = nrow(XY), byrow=T)
  # Z <- Z[nrow(Z):1,]
  
  image(x = XY[,1], y = XY[,2], z = Z, ...)
}




#' Plot Kriged Surface
#'
#' @param X a Kriged surface object from krige_surf. 
#' @param col.pal 
#' @param ... options to pass to spplot
#' @description Function will attempt to plot one or more kriged surfaces from krige_surf() 
#'
#' @return
#' @export
#'
#' @examples
plotKrige <- function(X, points = NULL, pcol = NA, ...){
  p <- list()
  
  for(i in 1:length(X)){
    if(!is.null(points)){
      
      p[[i]] <- spplot(X[[i]]$krige_output,
                       col.regions = viridis(100),
                       zcol = "var1.pred",
                       main = names(X)[i],
                       sp.layout = list(pts = list("sp.points", 
                                                   SpatialPoints(points),
                                                   pch=16,
                                                   col = cols
                       )))  
      
    } else{
      
      p[[i]] <- spplot(X[[i]]$krige_output,
                       col.regions = viridis(100),
                       zcol = "var1.pred",
                       main = names(X)[i])
    }
    
  }
  
  do.call(grid.arrange, p)
  
}





plot_fn_kr <- function(fn_kr, 
                       CEX = 0.5, ncols = 100, 
                       alpha= 0.3,
                       pch = 16,
                       pt.col = "black",
                       colorkey=T,
                       cex.lab = 1){
  
  fn_grid <- fn_kr$dataframes$grid
  fn_new_data <- fn_kr$dataframes$new_data
  
  
  sp.grid <- SpatialPointsDataFrame(coords = fn_grid[,1:2], fn_grid[,-c(1,2)])
  sp.new_data <- SpatialPointsDataFrame(coords = fn_new_data[,1:2], fn_new_data[,-c(1,2)])
  
  spplot(sp.grid,
         scales = list(draw = F),
         colorkey = colorkey,
         col.regions = viridis(ncols),
         sp.layout = list("sp.points", sp.new_data[,1:2], pch = pch, 
                          cex = CEX, col = pt.col, alpha = alpha))
  
  
}

