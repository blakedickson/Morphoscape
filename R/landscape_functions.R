

fill.grid <- function(X){
  
  full.grid <- expand.grid(unique(X[,1]),
                           unique(X[,2]))
  
  colnames(full.grid)[1:2] <- c("X","Y")
  colnames(X)[1:2] <- c("X","Y")
  
  X <- merge( X, full.grid, by = c("X", "Y"), all.x = T, all.y = T)
  
  X <- X[order(X$Y),]
  return(X)
}

scale.z <- function(Z) {
  
  Z <- Z - min(Z,na.rm = T)
  Z <- Z/max(Z,na.rm = T)
  return(Z)
}




# this function cooerces many functional traits into a 3d array.
# Data are coercced into the following structure:
# dims: 1= observations (warps), 2 = x,y,z coordinates, 3= functional traits

#' Coerces XY coordinate data and covariate data into a scaled dataframe, or optionally into
#'    a 3D array
#'
#' @param X A matrix with the first two columns containing the XY coordinates, and subsequent columns contianing trait data
#' @param row.names row names
#' @param func.names An optional string containing the names for traits given
#'     in z
#' @param array An optional logical argument to return a 3D array rather than a
#'     list. By default is set to FALSE
#' @param scale An optional logical argument to normalize z covariates to the
#'     same unit scale. Defaults to TRUE. Can be set to FALSE if the user wishes
#'     to normalize manually
#'
#' @return Returns a list, or optionally an Array. Each sheet contains an x,y,z
#'     functional dataframe corresponding to covariates given in z,
#'     labelled by 'func.names'
#' @export
#'
#' @examples X
fnc.dataframe <- function(X, row.names, func.names=NULL, array = F, scale = T){
  
  if( nrow(X) != nrow(expand.grid(unique(X[,1]), unique(X[,2]))) ){
    X <- fill.grid(X)
  }
  
  x <- X[,1] 
  y <- X[,2]
  z <- as.matrix(X[, -c(1:2)])
  
  
  
  if(is.null(func.names)){
    
    func.names <- colnames(X)[-c(1,2)]
    
  }    
  if (scale){
    z  <- apply(z, MARGIN = 2, FUN = scale.z)
  }
  
  # if(array){
  #   fnc.array <- array(dim=c(nrow(z), 3, ncol(z)),
  #                      dimnames = list(row.names,c("x","y","z"),func.names))
  #   
  #   for (i in 1:ncol(z)){
  #     fnc.array[,,i] <- cbind(x,y,as.numeric(z[,i]))
  #   }
  #   
  # } else{
    fnc.array <- data.frame(x,y,z)
    # names(fnc.array) <- func.names
    # attr(fnc.array, "class") <- "fnc.df"

  
  
  return(fnc.array)
}




#' Finds the top percentile of a W dataframe
#'
#' @param X A W dataframe
#' @param percentile upper percentage from which to sample
#' @param method "Quantile" or "Chi-squared"
#' @param sortby an optional input to choose which vector to sort by. Defaults to "Z"
#'
#' @return
#' @export
#'
#' @examples
gettop <- function(X, percentile = 0.05, method, sortby = "Z"){
  X <- X[ order( X[ , sortby], decreasing = T), ]
  if(method=="quantile"){
    X.top <- X[ X[ , sortby] > quantile( X[, sortby], probs = 1-percentile) , ]
  }
  if(method=="chi-squared"){
    critval <- qchisq(percentile, 2)
    x <- ( -2*( X[ , sortby] - X[ , sortby][1]) )
    X.top <- X[ 1:length( which( x < critval) ), ]
  }
  
  return(X.top)
}



#' Create a hull around input data. Allows for subsampling the total morphospace
#'
#' @param coords2D A matrix of coordinates from the input dataset
#' @param alpha 0-1 alpha value for hull formation.
#' @param plot Option to plot output hulled grid
#' @param resample resampling density. Defaults to 100
#'
#' @return returns a grid of coordinates
#' @export
#'
#' @examples
surf_hull <- function(coords2D, alpha = 1, plot = F, resample = 100){
  
  datahull <- ahull(coords2D[,1], coords2D[,2], alpha = 1)
  
  
  gridX <- seq(from = range(coords2D[,1])[1],
               to = range(coords2D[,1])[2],
               length = resample)
  
  gridY = seq(from = range(coords2D[,2])[1],
              to = range(coords2D[,2])[2],
              length = resample)
  
  grid2D <- expand.grid(x = gridX, y = gridY)
  
  hull.grid <- grid2D[inahull(datahull, p = as.matrix(grid2D)),]
  
  
  hull.grid <- as.data.frame(hull.grid)
  
  gridded(hull.grid) = ~x+y
  
  if(plot){
    
    par(mfrow = c(2,2))
    # plot(data_poly, main = "alpha hull")
    # plot(hull_coords[[1]], main = "alpha hull points")
    plot(grid2D, main = "resample grid")
    plot(hull.grid, main = "resample grid hull")
    
  }
  
  return(hull.grid)
  
}



#' Generate a surface by Kriging. 
#'
#' @param X A functional dataframe containing XY coordinates and scaled performance data.
#' @param hull Logical. Subset morphospace by creating a hull around input data
#' @param new_data XY coordinate data for points to calculate on the surface. This should contain all specimen coordinates or group means you wish to calculate in subsequent analyses
#' @param alpha 0-1 Alpha value to pass onto surf_hull
#' @param hullPlot Logical. Plot hull grid
#' @param resample resampling density of Kriged surfaces. Defaults to 100
#'
#' @return
#' @export
#'
#' @examples
krige_surf <- function(fnc_df, hull = T, new_data = NULL, alpha = 1, resample = 100){
  
  X <- na.omit(fnc_df)
  coords2D <- X[,1:2]
  new_NULL <- new_data
  
  if(hull){
    grid2D <- surf_hull(coords2D, alpha = alpha, resample = resample)
  } else{
    gridX <- seq(from = range(coords2D[,1])[1],
                 to = range(coords2D[,1])[2],
                 length = resample)
    
    gridY = seq(from = range(coords2D[,2])[1],
                to = range(coords2D[,2])[2],
                length = resample)
    
    grid2D <- expand.grid(x = gridX, y = gridY)
  }
  
  grid2D <- as.data.frame(grid2D)
  ngrid <- nrow(grid2D)

  
  Z <- data.frame(X[,3:ncol(X)])
  
  data_list <- list()
  for (i in 1:ncol(Z)){
    data_list[[i]] <- data.frame(x = X[,1],y = X[,2], z = Z[,i])
    coordinates(data_list[[i]]) = c(1,2)
    
  }
  
  names(data_list)<- colnames(Z)
  
  krig_grid <- suppressWarnings(lapply(data_list, FUN = autoKrige, new_data = SpatialPoints(grid2D)))
  kriged_fn_df_grid <- cbind(as.data.frame(grid2D),
                             sapply(krig_grid, FUN = function(X){
                               as.data.frame(X$krige_output)[,3]
                             }
                             ))
  
  
  
  
  kriged_fn_df_grid[,-c(1,2)]  <- apply(kriged_fn_df_grid[,-c(1,2)], MARGIN = 2, FUN = scale.z)
  
  
  
  if(!is.null(new_data)){
      new_data <- as.matrix(new_data)
  
    # new_data <- rbind(as.matrix(grid2D), new_data)
    new_data <- SpatialPoints(new_data)
    krig_new_data <- suppressWarnings(lapply(data_list, FUN = autoKrige, new_data = new_data))
    
    kriged_fn_df_newdata <- cbind(as.data.frame(new_data),
                                  sapply(krig_new_data, FUN = function(X){
                                    as.data.frame(X$krige_output)[,3]
                                  }
                                  ))
    kriged_fn_df_newdata[,-c(1,2)]  <- apply(kriged_fn_df_newdata[,-c(1,2)], MARGIN = 2, FUN = scale.z)
    
    surfaces <- list(autoKrige = krig_grid,
                     dataframes = list(grid = kriged_fn_df_grid,
                                       new_data = kriged_fn_df_newdata) )
    
  }else{
    
    surfaces <- list(autoKrige = krig_grid,
                     dataframes = list(grid = kriged_fn_df_grid))
  }
  
  
  
  
  # X<- krig_list$EXP_SE
  

  # krigedgrid = kriged_fn_df[1:ngrid,]
  
  # krigednew_data = kriged_fn_df[(ngrid+1):nrow(kriged_fn_df),]
  
  # surfaces <- list(autoKrige = krig_list,
  #                  dataframes = list(grid = krigedgrid,
  #                                    new_data = krigednew_data) )
  
  
  # plot_fn_kr(surfaces)
  
  
  class(surfaces) <- "kriged_surfaces"
  
  return(surfaces)
  
}




#' Calculate a Zprime surface for a given vector of W
#'
#' @param W 
#' @param fnc_data A dataframe containing performance traits and coordinate data
#' @param rekrige Optional. Rekrige output Zprime
#'
#' @return
#' @export
#'
#' @examples
calc.W.kr <- function(W, fnc_data, rekrige = F){
  
  names(W) <- names(fnc_data$grid[,3:ncol(fnc_data$grid)])
  
  Wprime <- lapply(fnc_data, FUN = function(data,W){
    XY <- data[,1:2]
    FN <- data[,3:ncol(fnc_data$grid)]
    FNw <- sweep(FN, 2, W, FUN= "*")
    Z <- rowSums(FNw)
    Wprime <- cbind(FNw, Z)
    
    
    Wprime <- cbind(XY, Wprime)
    
  },W)
  
  return(list(W = W, Wprime = Wprime))

}


#' Calculate all weighted landscapes
#'
#' @param weights a dataframe of weight combinations generated by generate.weights()
#' @param fnc_data a dataframe of functional traits. The first two columns should represent the XY coordinates, with subsequent columns representing scaled trait data.
#' @param new_data an optional matrix of coordinate data to interpolate on landscapes. If calculation of group landscapes is desired, all datapoints must be provided here
#' @param plot currently not functional. Logical. An option to plot kriged performance surfaces from fnc_data
#' @param save.all Logical. Save results to file. Recommended for large weights dataframes.
#' @param verbose currently not functional. Option to export all data
#' @param file.out Optional. Output name for saved results
#' @param ... Optional parameters to pass onto krige_surf
#'
#' @return Returns a list containing $kriged_fnc a list of autoKrige surfaces for each performacne trait; 
#' $fn_dataframe a dataframe of resampled grid points and new_data points on performacne surfaces;
#' $all_Wprime_surfs a list containing resampled grid and new_data points for all weighted landscapes
#' @export
#'
#' @examples
calc.all.lscps <- function(weights, fnc_data, new_data = NULL,
                           save.all = T,  verbose = T,
                           file.out = "allLandscapes", ...){
  
  colnames(new_data) <- c("x", "y")
  
  if(is.data.frame(fnc_data)){
    # fnc_data <- krige_surf(X = fnc_data, new_data = new_data)
    kr_data <- krige_surf(fnc_data, new_data = new_data)
  }
  
  if(class(kr_data)== "kriged_surfaces"){
    
    fn_dataframe <- kr_data$dataframes
    
  } else(stop("data is not a dataframe or kriged_surfaces object"))
  
  # rownames(rawdata$EXP_SE)
  
  
  
  
  # all_Wprime_surfs <-  apply(weights, MARGIN = 1, FUN = calc.W.kr,
  #                            fnc_data = fn_dataframe)
  
  all_Wprime_surfs <-  apply(weights, MARGIN = 1, FUN = calc.W.kr,
                             fnc_data = fn_dataframe, ...)
  
  
  
  all_Lscape_data <- list(fnc_data = fnc_data,
                          kriged_fnc_surfs = kr_data, 
                          fn_dataframe = fn_dataframe, 
                          all_Wprime_surfs = all_Wprime_surfs)
  
  
  
  
  if(save.all){
    if(!dir.exists("./landscapeResults")){
      dir.create("./landscapeResults")
    }
    
    fp = paste("./landscapeResults", "/",
               file.out, ".Rdata", sep = "")
    
    if(file.exists(fp)){
      file.remove(fp)
    }
    save(all_Lscape_data, file = fp)
  }
  
  return(all_Lscape_data)
  
}


#' Workhorse function for generate.weights. Used to flexibly generate combinations of weights.
#'
#' @param n 
#' @param k 
#'
#' @return
#' @export
#'
#' @examples
parti <- function(n, k) {
  if (n < 0) {
    message("error: n<0")
    return(NA)
  }
  if (k == 1)
    return(matrix(n, 1, 1))
  M <- cbind(parti(n, k - 1), 0)
  if (n > 0)
    for (i in (1:n)) M <- rbind(M, cbind(parti(n - i, k - 1), i))
  M
  
}

#' Generate a generic dataframe containing weight combinations. Can be used in
#'   search.w.exhaustive, or to split jobs for distributed computing
#'
#' @param step Numeric. Degree by which to vary weights by.
#' @param nvar Numeric. Number of variables (columns) to generate
#' @param varnames Optional. Names for variables
#' @param verbose Logical. Return dataframe. Used for silencing in time.est
#' @param ...
#'
#' @return
#' @export
#'
#' @examples X
generate.weights <- function(step, nvar, varnames = NULL, 
                             verbose = T, Fn = NULL, ...) {
    n = 1/step
    weights<- parti(n, nvar)/n

    if (!is.null(varnames)){
        colnames(weights) <- varnames
    }  else colnames(weights) <- 1:nvar

    print(paste(nrow(weights),"rows generated"))

    if (verbose) return(weights)
}

#' Estimate length of time to perform exhaustive search
#'
#' @param num.perm number of permutations (rows of weights)
#' @param nvar number of variables
#' @param Fn multi.func surfaces object
#' @param xmar,ymar set X and Y margins
#'
#' @return Returns an estimate time
#' @export
#'
#' @examples X
time_est <- function(num.perm, nvar, Fn, xmar, ymar) {

    if (length(Fn[[1]]) == 3) {
        Fn.tmp <- list()
        for (i in 1:length(Fn)) {
            Fn.tmp[[i]] <- Fn[[i]]$poly

        }
        names(Fn.tmp) <- names(Fn)
        Fn <- Fn.tmp

    }

    apply(X = matrix(nrow = 10, ncol = nvar, 1), MARGIN = 1, FUN = lik_Zprime, Zprime = c(0, 0), Fn = Fn, xmar = xmar, ymar = ymar)

    t100 <- system.time(apply(X = matrix(nrow = 100, ncol = nvar, 1), MARGIN = 1, FUN = lik_Zprime, Zprime = c(0, 0), Fn = Fn, xmar = xmar, ymar = ymar))
    t1.sec <- (t100[3]/100)
    t.est <- t1.sec * num.perm
    print(paste(t1.sec, "seconds for one calculation. Estimated time for full dataset: ", t.est, "seconds"))
    return(t.est)
}


getwn <- function(X, index){
  Zprime <- data.frame(t(as.matrix(X[[1]])),as.matrix(X$Wprime$new_data[index,c("x","y","Z")]))
}

#' Extract new_data points from a wn list
#'
#' @param X a Wprime list from calc.all.lscps
#' @param index an index of new_data points to extract
#'
#' @return Returns a dataframe if index = 1, else a list of points for each wn
#' @export
#'
#' @examples
getNew_dataW <- function(X, index){
  newDat <- lapply(X, getwn, index)
  return(newDat)
  
}



#' Calculate Wprime for a given index of new_data
#'
#' @param index a vector containing an index of new_data points
#' @param X a Wprime list from calc.all.lscps
#' @param method c("chi-squared", "max") method to calculate Wprime. Defaults to "chi-squared"
#' @param percentile upper percentile which to calculate Wprime if using method = "chi-squared"
#' @param verbose logical. If TRUE, Also returns a full list of Z values for index
#'
#' @return Returns a list containing Wprime, and W, a dataframe of all calculated landscape heights.
#' @description Calculate the best landscape (Zprime) for a given index of new_data. If index is greater than 1 then Z is calculated for all points and averaged. Returns Zprime and a dataframe containing weights, group mean coordinates and Z heights, If index is a single point, returns Zprime and a dataframe of heights for the single point.   
#' @export
#'
#' @examples
calcGrpWprime <- function(index, X, method = "chi-squared", percentile = 0.05, verbose = T){
  
  
  surfs <- X$all_Wprime_surfs
  fn_dataframe <- X$fn_dataframe
  
  
  Wlist <- getNew_dataW(surfs, index)
  tmp <- do.call(rbind,lapply(Wlist, function(X) colMeans(X)))
  tmp <- tmp[ order( tmp[ , "Z"], decreasing = T), ]
  
  if(method =="max"){
    max <- tmp[which.max(tmp[,"Z"]),]
    
  } 
  
  if(method == "chi-squared"){
    
    X.top <- gettop(tmp, percentile = percentile, method = method,
                    sortby = "Z")
    if(is.matrix(X.top)){
      max <- colMeans(X.top)
    } else{
      max <- X.top
      
    }
    
  }
  # 
  
  
  
  
  
  Zprime <- colMeans( X.top[,-match(c("x","y"), colnames(X.top))] )
  wn.se <- apply(X.top[,-match(c("x","y"), colnames(X.top))],2, plotrix::std.error)
  wn.sd <- apply(X.top[,-match(c("x","y"), colnames(X.top))],2, sd)
  wn.range <- apply(X.top[,-match(c("x","y"), colnames(X.top))],2, range)
  
  
  Wprime <- calc.W.kr(W = Zprime[-match(c("Z"), names(Zprime))], fnc_data = fn_dataframe)
  
  
  
  if(verbose){
    return(list(list(Zprime=Zprime, wn.se=wn.se, wn.sd=wn.sd, wn.range=wn.range), W = tmp, Wlist = Wlist))
    # return(list(list(Zprime=Zprime, wn.se=wn.se, wn.sd=wn.sd, wn.range=wn.range), W = tmp, Wlist = Wlist))
    
  }else{
    return(list(Zprime = list(wn=Zprime, wn.se=wn.se, wn.sd=wn.sd, wn.range=wn.range), Wprime = Wprime,  W = tmp))  }
  
}


# THIS FUNCTION IS CURRENTLY NON-FUNCTIONAL

#' Calculate the best model fits
#'
#' @param X A weights matrix generated from search.w.exhaustive()
#' @param sortby c("z"). Which optimization vector to sort by.
#'   Defaults to "lik"
#' @param percentile percentile (alpha) cuttoff. Defaults to 0.99 (0.01)
#' @param method method by which to calculate top fits. Method = "quantile"
#'   simply caclulates the average weights from top percentile set by the user.
#'   Method = "chi-squared" calculates the best significant models based on the
#'   log-likelihood ratio distribution.
#' @author Blake Dickson and Katrina Jones
#' @return A list containing wn the best averaged model weights, wn.se the
#'   standard error, wn.sd te standard deviation and wn.range, the range of top
#'   wn values
#' @export
#'
#' @examples X
calc.best.wn <- function(X, sortby = "Z", percentile = 0.99, method=c("quantile")){
    X <- X[ order( X[ , sortby], decreasing = T), ]
    if(method=="quantile"){
        X.top <- X[ X[ , sortby] > quantile( X[, sortby], probs = percentile) , ]
    }
    if(method=="chi-squared"){
        critval <- qchisq(1 - percentile, 1)
        x <- ( -2*( X$lik - X$lik[1]) )
        X.top <- X[ 1:length( which( x < critval) ), ]
    }
    wn <- colMeans( X.top[, -match(c("z", "lik"), colnames(X))] )
    wn.se <- apply(X.top[,-match(c("z", "lik"), colnames(X))],2, plotrix::std.error)
    wn.sd <- apply(X.top[,-match(c("z", "lik"), colnames(X))],2, sd)
    wn.range <- apply(X.top[,-match(c("z", "lik"), colnames(X))],2, range)
    # wn <- colMeans( X.top[, -match(c("z", "lik", "dis"), colnames(X))] )
    # wn.se <- apply(X.top[,-match(c("z", "lik", "dis"), colnames(X))],2, plotrix::std.error)
    # wn.sd <- apply(X.top[,-match(c("z", "lik", "dis"), colnames(X))],2, sd)
    # wn.range <- apply(X.top[,-match(c("z", "lik", "dis"), colnames(X))],2, range)
    return(list(wn=wn, wn.se=wn.se, wn.sd=wn.sd, wn.range=wn.range))
}






#' 
#' 
#' require("alphahull")
#' require("hull2spatial")
#' require("sp")
#' require("automap")
#' 
#' #' Surface Kriging
#' #'
#' #' @param X A Functional Dataframe. Columns 1,2 should contain XY morphospace coordinates, with subsequent columns containing functional data correspoinding to morphospace points.
#' #' @param hull optional logical. If TRUE, function will only Krige area within the alpha hull as calculated by surf_hull
#' #' @param alpha optional. Alpha value for surf_hull
#' #' @param hullPlot Logical. Plot hull?
#' #' @param ... additional parameters to pass onto autoKrige
#' #'
#' #' @return returns 
#' #' @export
#' #'
#' #' @examples
#' function(X, hull = F, alpha = 0.001, hullPlot = F,...){
#'   
#'   X <- na.omit(X)
#'   
#'   if(!is.null(na.action(X))){
#'     
#'     warning("NAs detected, excluding rows ", paste(na.action(X), collapse = ","))
#'     
#'   }
#'   
#'   coords2D <- X[,1:2]
#'   
#'   
#'   # if(hull){
#'   #   grid2D <- surf_hull(coords2D, alpha = alpha, plot = hullPlot)
#'   #   
#'   # } else{
#'   #   gridX <- seq(from = range(coords2D[,1])[1],
#'   #                to = range(coords2D[,1])[2],
#'   #                length = 100)
#'   #   
#'   #   gridY = seq(from = range(coords2D[,2])[1],
#'   #               to = range(coords2D[,2])[2],
#'   #               length = 100)
#'   #   
#'   #   grid2D <- as.data.frame(expand.grid(x = gridX, y = gridY))
#'   #   
#'   #   gridded(grid2D) = ~x+y
#'   # }
#'   
#'   Z <- cbind(X[,3:ncol(X)])
#'   
#'   data_list <- list()
#'   for (i in 1:ncol(Z)){
#'     data_list[[i]] <- data.frame(x = X[,1],y = X[,2], z = Z[,i])
#'     coordinates(data_list[[i]]) = c(1,2)
#'     
#'   }
#'   
#'   names(data_list)<- colnames(Z)
#'   
#'   
#'   
#'   krig_list <- lapply(data_list, FUN = autoKrige, ...)
#'   
#'   return(krig_list)
#'   
#' }
#' 


#' Create alpha hull around data
#'
#' @param coords2D a matrix of 2D XY morphospace coordinates
#' @param alpha alpha value for creating hulls
#' @param plot logical. Plot alpha hull on morphospace
#'
#' @return returns a grid of points within the alpha hull. 
#' @export
#'
#' @examples
surf_hull <- function(coords2D, alpha = 1, plot = F, resample = 1000){
  
  datahull <- ahull(coords2D[,1], coords2D[,2], alpha = 1)
  
  # data_poly <- ahull2poly(datahull)
  
  # hull_coords <- list(data_poly@polygons[[1]]@Polygons[[1]]@coords)
  
  ## define grid
  
  gridX <- seq(from = range(coords2D[,1])[1],
               to = range(coords2D[,1])[2],
               length = resample)
  
  gridY = seq(from = range(coords2D[,2])[1],
              to = range(coords2D[,2])[2],
              length = resample)
  
  grid2D <- expand.grid(x = gridX, y = gridY)
  
  
  
  hull.grid <- grid2D[inahull(datahull, p = as.matrix(grid2D)),]
  
  
  hull.grid <- as.data.frame(hull.grid)
  
  gridded(hull.grid) = ~x+y
  
  if(plot){
    
    par(mfrow = 2,2)
    plot(data_poly, main = "alpha hull")
    plot(hull_coords[[1]], main = "alpha hull points")
    plot(grid2D, main = "resample grid")
    plot(hull.grid, main = "resample grid hull")
    
  }
  
  return(hull.grid)
  
}




