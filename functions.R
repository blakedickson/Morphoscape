


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
    fnc.array <-  as.data.frame(cbind(x,y,z))
  #   
  # }
    
    
  
    # names(fnc.array) <- func.names
    # attr(fnc.array, "class") <- "fnc.df"

  
  return(fnc.array)
}


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


# calc.list<- function(X, Y){
#   B <- X[[1]] 
#   B <- c(B,mean(X[[2]][g,3]))
#   
#   return(B)
# }

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


# 
# calc.best.wn <- function(X, sortby, percentile = 0.01, plot = F, plotTitle = "", 
#                          method=c("quantile")){
#   X <- X[ order( X[ , sortby], decreasing = T), ]
#   # if(method=="quantile"){
#   #   X.top <- X[ X[ , sortby] > quantile( X[, sortby], probs = percentile) , ]
#   # }
#   # if(method=="chi-squared"){
#   #   critval <- qchisq(1 - percentile, 2)
#   #   x <- ( -2*( X[ , sortby] - X[ , sortby][1]) )
#   #   X.top <- X[ 1:length( which( x < critval) ), ]
#   # }
#   # 
#   
#   X.top <- gettop(X, percentile, method)
#   
#   if(plot){
#     plot(X[,5], main = plotTitle)
#     lines(x = c(0,nrow(X)),
#           y= c(min(X.top[,sortby]),min(X.top[,sortby])),
#           col = "red")
#     
#   }
#   
#   
#   wn.top <- X.top[1,]
#   wn.mean <- colMeans( as.matrix(X.top) )
#   wn.se <- apply(as.matrix(X.top),2, plotrix::std.error)
#   wn.sd <- apply(as.matrix(X.top),2, sd)
#   wn.range <- apply(as.matrix(X.top),2, range)
#   # wn <- colMeans( X.top[, -match(c("z", "lik", "dis"), colnames(X))] )
#   # wn.se <- apply(X.top[,-match(c("z", "lik", "dis"), colnames(X))],2, plotrix::std.error)
#   # wn.sd <- apply(X.top[,-match(c("z", "lik", "dis"), colnames(X))],2, sd)
#   # wn.range <- apply(X.top[,-match(c("z", "lik", "dis"), colnames(X))],2, range)
#   return(list(wn.top= wn.top, wn.mean=wn.mean, wn.se=wn.se, wn.sd=wn.sd, wn.range=wn.range))
# }
# 
# 
# calc.all <- function(weights, data, coords){
#   apply(weights, MARGIN = 1, FUN = calc.W.kr, data = data, Zprime = coords )
#   
# }

# 
# data <- exp_df
# 
# Z <- as.matrix(data[,3:6])
# Wprime <- cbind(data[,1:2], Z)
# X <- na.omit(Wprime)
# 
# X <- na.omit(X)
# coords2D <- X[,1:2]
# alpha = 1
# plot = T
# resample = 100

# surf_hull <- function(coords2D, alpha = 0.5, plot = F, resample = 100){
#   
#   datahull <- ahull(coords2D[,1], coords2D[,2], alpha = alpha)
#   
#   data_poly <- ahull2poly(datahull)
#   
#   hull_coords <- list(data_poly@polygons[[1]]@Polygons[[1]]@coords)
#   
#   ## define grid
#   
#   gridX <- seq(from = range(coords2D[,1])[1],
#                to = range(coords2D[,1])[2],
#                length = resample)
#   
#   gridY = seq(from = range(coords2D[,2])[1],
#               to = range(coords2D[,2])[2],
#               length = resample)
#   
#   grid2D <- expand.grid(x = gridX, y = gridY)
#   
#   
#   
#   hull.grid <- grid2D[inahull(datahull, p = as.matrix(grid2D)),]
#   
#   
#   hull.grid <- as.data.frame(hull.grid)
#   gridded(hull.grid) = ~x+y
#   
#   if(plot){
#     
#     par(mfrow = 2,2)
#     plot(data_poly, main = "alpha hull")
#     plot(hull_coords[[1]], main = "alpha hull points")
#     plot(grid2D, main = "resample grid")
#     plot(hull.grid, main = "resample grid hull")
#     
#     
#   }
#   
#   return(hull.grid)
#   
# }

surf_hull <- function(coords2D, alpha = 1, resample = 1000){
  
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
  
  # if(plot){
  #   
  #   par(mfrow = c(2,2))
  #   # plot(data_poly, main = "alpha hull")
  #   # plot(hull_coords[[1]], main = "alpha hull points")
  #   plot(grid2D, main = "resample grid")
  #   plot(hull.grid, main = "resample grid hull")
  #   
  # }
  # 
  return(hull.grid)
  
}

# X = exp_df
# new_data = rbind(c(0.001, 0.002),
#                  c(0.002, 0.001))
# 
# alpha = 1
# hullPlot = F
# resample = 100
# hull = T

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

  if(!is.null(new_data)){
    if(!is.matrix(new_data)){
      new_data <- matrix(new_data, ncol = 2)
    }

    # new_data <- rbind(as.matrix(grid2D), new_data)
    new_data <- SpatialPoints(new_data)
  } 

  Z <- data.frame(X[,3:ncol(X)])

  data_list <- list()
  for (i in 1:ncol(Z)){
    data_list[[i]] <- data.frame(x = X[,1],y = X[,2], z = Z[,i])
    coordinates(data_list[[i]]) = c(1,2)

  }

  names(data_list)<- colnames(Z)

  krig_grid <- suppressWarnings(lapply(data_list, FUN = autoKrige, new_data = SpatialPoints(grid2D)))
  krig_new_data <- suppressWarnings(lapply(data_list, FUN = autoKrige, new_data = new_data))
  

  
  # X<- krig_list$EXP_SE

  kriged_fn_df_grid <- cbind(as.data.frame(grid2D),
          sapply(krig_grid, FUN = function(X){
            as.data.frame(X$krige_output)[,3]
          }
          ))

    kriged_fn_df_newdata <- cbind(as.data.frame(new_data),
          sapply(krig_new_data, FUN = function(X){
            as.data.frame(X$krige_output)[,3]
          }
          ))

    
    
    kriged_fn_df_grid[,-c(1,2)]  <- apply(kriged_fn_df_grid[,-c(1,2)], MARGIN = 2, FUN = scale.z)
    kriged_fn_df_newdata[,-c(1,2)]  <- apply(kriged_fn_df_newdata[,-c(1,2)], MARGIN = 2, FUN = scale.z)
    

  # krigedgrid = kriged_fn_df[1:ngrid,]

  # krigednew_data = kriged_fn_df[(ngrid+1):nrow(kriged_fn_df),]

  # surfaces <- list(autoKrige = krig_list,
  #                  dataframes = list(grid = krigedgrid,
  #                                    new_data = krigednew_data) )
  
  surfaces <- list(autoKrige = krig_grid,
                   dataframes = list(grid = kriged_fn_df_grid,
                                     new_data = kriged_fn_df_newdata) )
  
  # plot_fn_kr(surfaces)
  

  class(surfaces) <- "kriged_surfaces"

  return(surfaces)

}

# W <- weights[1,]
# fnc_data = fn_dataframe
# method = "sum"
# rekrige = F

calc.W.kr <- function(W, fnc_data, rekrige = F, ...){
  
  names(W) <- names(fnc_data$grid[,3:ncol(fnc_data$grid)])
  
  # data <- fnc_data$grid
  
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

# 
# fnc_data <- exp_df
# new_data <- coords
# plot = F
# save.all = T
# file.out = "allLandscapes"




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







#' Significance test between group weights
#'
#' @param grpa,grpb Weights dataframes generated from search.w.exhaustive()  
#' @param percentile Numeric. Determines the top percentile of group weights. Defaults to 0.01
#' @author Katrina Jones
#' @return
#' @export
#'
#' @examples X
lands.grp.test <- function(grpa, grpb, percentile = 0.05){
  #get top 1% based on likelihood
  
  
  
  
  besta <- gettop(grpa$W, percentile, method="quantile")
  bestb <- gettop(grpb$W, percentile, method="quantile")
  
  # besta <- grpa[order(grpa$lik,decreasing = T), ][1:round(nrow(grpa)
  #                                                         * percentile), ]
  # bestb <- grpb[order(grpb$lik,decreasing = T), ][1:round(nrow(grpb)
  #                                                         * percentile), ]
  #Check for matching models
  m <- match(rownames(besta), rownames(bestb))
  n.match <- length(m[!is.na(m)])
  p.match <- n.match / length(m)
  return(list(n.match = n.match, p.val = p.match, 
              matching = besta[na.omit(m), ]))
}

#groups list of weighting files
#' Tests for pairwise signifince between mulitple group weights
#'
#' @param groups A list of GrpWprime objects from calcGrpWprime()
#' @param percentile Numeric. Determines the top percentile of group weights. Defaults to 0.01
#' @author Katrina Jones
#' @return Returns a matrix of pairwise group comparisons, with the upper triangle containing counts and the lower triangle probabilities
#' @export
#'
#' @examples X
multi.lands.grp.test<-function(groups, percentile=0.05, method = "chi-squared"){
  tests <- combn(names(groups),2)
  res <- data.frame(matrix(NA, nrow = ncol(tests), ncol = 4))
  colnames(res) <- c("grpa", "grpb", "n matches", "p")
  #run tests pairwise
  for(i in 1:ncol(tests)){
    grpa <- groups[tests[, i][1]][[1]]
    grpb <- groups[tests[, i][2]][[1]]
    res[i, "grpa"] <- tests[, i][1]
    res[i, "grpb"] <- tests[, i][2]
    #run test
    t <- lands.grp.test(grpa, grpb, percentile=percentile)
    res[i, "n matches"] <- t$n.match
    res[i, "p"] <- t$p.val
  }
  #Make nice table
  tab<-matrix(NA, nrow = length(names(groups)), ncol = length(names(groups)),
              dimnames =list(names(groups), names(groups)))
  tab[lower.tri(tab)] <- round(res$"n matches")
  mat <- t(tab)
  tab[upper.tri(tab)] <- mat[upper.tri(mat)]
  tab[lower.tri(tab)] <- round(res$p, 3)
  return(tab)
}



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

generate.weights <- function(step, nvar, varnames = NULL, time.est = F,
                             verbose = T, Fn = NULL, ...) {
  n = 1/step
  weights<- parti(n, nvar)/n
  
  if (!is.null(varnames)){
    colnames(weights) <- varnames
  }  else colnames(weights) <- 1:nvar
  
  rownames(weights) <- 1:nrow(weights)
  
  print(paste(nrow(weights),"rows generated"))
  
  if (time.est) {
    xmar<-range(Fn[[1]][[1]]$x)
    ymar<-range(Fn[[1]][[1]]$y)
    time_est(num.perm = nrow(weights), nvar = nvar, Fn = Fn,
             xmar = xmar, ymar = ymar)
  }
  
  if (verbose) return(weights)
}

krige_wn_surf <- function(wn,data, lik = T){
  
  if(lik){
    wn <- wn[-length(wn)]  
    
  }
  
  FN <- as.matrix(data[,3:6])
  Z <- rowSums(sweep(FN, 2, wn, FUN= "*"))
  Wprime <- cbind(data[,1:2], Z)
  
  Wkr<- krige_surf(na.omit(Wprime), 
                   hull = T, resample = 100)[[1]]
  
  return(Wkr)
}

summary.wn <- function(groups, return = "all"){
  
  if(return == "all"){
    ret <- lapply(groups, FUN = function(X) {return(t(data.frame(wn.top=X$wn$wn.top,
                                                                 wn.mean=X$wn$wn.mean,
                                                                 wn.se=X$wn$wn.se,
                                                                 wn.sd=X$wn$wn.sd,
                                                                 wn.rangelower=X$wn$wn.range[1,],
                                                                 wn.rangeupper=X$wn$wn.range[2,]
    ))
    )
    } )
  } else(
    ret <- t(data.frame(lapply(groups, FUN = function(X) {return(X$wn[[return]])} )))
    
  )
  
  return(ret)
  
}


t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  invisible(t.col)
}

