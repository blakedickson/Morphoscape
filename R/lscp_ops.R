
require("Morphoscape")

data("turtles")
data("warps")

fnc_df <- as_fnc_df(warps, 
                       func.names = c("hydro", "curve", "mech", "fea"))

kr_surf <- krige_surf(warps_fnc, new_data = turtles)



grid_weights <- generate_weights(n = 20, data = kr_surf)

all_lscps <- calc_all_lscps(kr_surf,
                            grid_weights = grid_weights)
all_lscps


wprime_S <- calcGrpWprime(all_lscps,
                          index = Ecology == "S")

wprime_T <- calcGrpWprime(all_lscps,
                          index = Ecology == "T")

attributes(wprime_T)

attr(wprime_S, "by_name")

plot(wprime_S)
plot(wprime_T)

wprime_S$Wprime$Wprime$grid[,c("x", "y", "Z")]

# Sum any number of adaptive landscapes
#'
#' @param landscapes a list of adaptive lanscapes
#'
#' @return Returns a combined adpative landscape
#' @export
#'
#' @examples X


lscps <- list(wprime_S, wprime_T)
names(lscps)

names(lscps) <- alist(wprime_S, wprime_T)
names(lscps)


L <- as.list(lscps)

class(L[[l]])



  
X <- wprime_S
kr_surf$dataframes


getWprime <- function(X){
  
  if(class(X) == "grp_Wprime"){
    
    grid <- X$Wprime$Wprime$grid[,c("x", "y", "Z")]
    new_data <- X$Wprime$Wprime$new_data[,c("x", "y", "Z")]
    
    Wprime <- list(grid = grid, new_data = new_data)
    class(Wprime) <- "wprime"
  } 
  
  if(class(X) == "wtd_lscp"){
    
  }
  if(class(X) == "poly.surf"){
    
  }
  
  return(Wprime)
  
}
  
sum_lscps <- function(lscps, average = TRUE) {
  
  if(!is.list(lscps)){
    stop("'lscps' is not a named list")
    
  }
  if(is.null(names(lscps))){
    warning("Provided list objects are not named")
    
  }
  names(lscps)
  
  Wprimes <- list()
    
    for (l in 1:length(lscps)) {
      
      if(!any(class(lscps[[l]]) == c("grp_Wprime", "wtd_lscp", "poly.surf", 
                                 "data.frame", "matrix"))){
        stop(cat("Object", l ,"is not a dataframe or Morphoscape object containing a wprime surface"))
      }
         
      if(is.data.frame(lscps[[l]]) || is.matrix(lscps[[l]])){
        if(ncol(lscps[[l]]) == 3){
          Wprimes[[l]]$grid <- lscps[[l]]
          
          } else{
          stop(cat("Dataframe", l ,"must have 3 columns corresponding to XYZ spatial data"))
        }
        
      } else{
        Wprimes[[l]] <- getWprime(lscps[[l]])
        
      }
    }
  
  
  grids  <- lapply(Wprimes, FUN = function(X) X$grid)
  
  #check grid points match
  for(i in 1:length(grids)){
    
    if(!all(grids[[1]][,1:2] == grids[[i]][,1:2])){
      stop("Grid points do not match between objects 1 and ", i)
    }
  }
  
  
  grid.sum <- Reduce("+", grids)
  
  # if(!is.null(new_data)){
  #   if(ncol(new_data) !=2){
  #     stop(cat("Provided new_data must have 2 columns corresponding to XY spatial coordinates"))
  #   }
  # }
  # 
  # if(is.null(new_data)){
  #   if(any(unlist(lapply(Wprimes, inherits,"wprime" )))){
  #     
  #     new_data  <- Wprimes[[which(unlist(lapply(Wprimes, inherits,"wprime" )))[1]]]$new_data[,1:2]
  #     
  #   } else{
  #     new_data = NULL
  #   }
  #   
  # }
  
  class(grid.sum) <- "combined.surface"
  attr(grid.sum, "parents") <-  names(lscps)
  attr(grid.sum, "operation") <- "summed"
  attr(grid.sum, "averaged") = average
  
  return(grid.sum)
  
  
}


summed_surfs <- sum_lscps(list(wprimeS = wprime_S, wprime_T = wprime_T))



summed_surfs


div_lscps <- function(lscps, binary = F) {
    if(length(lscps) != 2){
      stop("lscps should be a named list of length 2")
    }  
  
  if(is.null(names(lscps))){
    warning("Provided list objects are not named")
    
  }
  
  
  Wprimes <- list()
  
  for (l in 1:length(lscps)) {
    
    if(!any(class(lscps[[l]]) == c("grp_Wprime", "wtd_lscp", "poly.surf", 
                                   "data.frame", "matrix"))){
      stop(cat("Object", l ,"is not a dataframe or Morphoscape object containing a wprime surface"))
    }
    
    if(is.data.frame(lscps[[l]]) || is.matrix(lscps[[l]])){
      if(ncol(lscps[[l]]) == 3){
        Wprimes[[l]]$grid <- lscps[[l]]
        
      } else{
        stop(cat("Dataframe", l ,"must have 3 columns corresponding to XYZ spatial data"))
      }
      
    } else{
      Wprimes[[l]] <- getWprime(lscps[[l]])
      
    }
  }
  
  Az <- Wprimes[[1]]$grid$Z
  Bz <- Wprimes[[2]]$grid$Z
  

  L <- Wprimes[[1]]$grid
  
    L$z <- (Az + 1)/(Bz + 1)

    if (binary) {
        bn <- L$z > 1
        L$z[bn] <- 1
    }
    class(L) <- "combined.surface"
    attr(L, "binary") <-  binary
    attr(L, "parents") <-  names(lscps)
    attr(L, "operation") <- "divided"
    
    
    
    return(L)
}


div_lscps(lscps)




sub_lscps <- function(lscps, binary = F) {
  if(length(lscps) != 2){
    stop("lscps should be a named list of length 2")
  }  
  
  if(is.null(names(lscps))){
    warning("Provided list objects are not named")
    
  }
  
  
  Wprimes <- list()
  
  for (l in 1:length(lscps)) {
    
    if(!any(class(lscps[[l]]) == c("grp_Wprime", "wtd_lscp", "poly.surf", 
                                   "data.frame", "matrix"))){
      stop(cat("Object", l ,"is not a dataframe or Morphoscape object containing a wprime surface"))
    }
    
    if(is.data.frame(lscps[[l]]) || is.matrix(lscps[[l]])){
      if(ncol(lscps[[l]]) == 3){
        Wprimes[[l]]$grid <- lscps[[l]]
        
      } else{
        stop(cat("Dataframe", l ,"must have 3 columns corresponding to XYZ spatial data"))
      }
      
    } else{
      Wprimes[[l]] <- getWprime(lscps[[l]])
      
    }
  }
  
  Az <- Wprimes[[1]]$grid$Z
  Bz <- Wprimes[[2]]$grid$Z
  
  
  L <- Wprimes[[1]]$grid
  
  L$z <- Az - Bz
  
  if (binary) {
    bn <- L$z > 0
    L$z[bn] <- 1
  }
  class(L) <- "combined.surface"
  attr(L, "binary") <-  binary
  attr(L, "parents") <-  names(lscps)
  attr(L, "operation") <- "subtracted"
  
  
  
  return(L)
}

sub_lscps(lscps)




mult_lscps <- function(lscps, num = NULL) {
  if(length(lscps) != 2){
    stop("lscps should be a named list of length 2")
  }  
  
  if(is.null(names(lscps))){
    warning("Provided list objects are not named")
    
  }
  
  
  Wprimes <- list()
  
  for(l in 1:length(lscps)){
    if(!any(class(lscps[[l]]) == c("grp_Wprime", "wtd_lscp", "poly.surf", 
                                   "data.frame", "matrix"))){
      stop("lscps should contain spatial datasets of class grp_Wprime, wtd_lscp, poly.surf, 
                                   data.frame or matrix")
  }
    if(is.data.frame(lscps[[l]]) || is.matrix(lscps[[l]])){
      if(ncol(lscps[[l]]) == 3){
        Wprimes[[l]]$grid <- lscps[[l]]
        
      } 
    } else{
      Wprimes[[l]] <- getWprime(lscps[[l]])
    }
  } 

  
  
  if(!is.null(num)){ #multiply landscapes by numeric vector
    if(length(num) == 1){
      
      if(length(Wprimes) == length(num)){
        for(ii in 1:length(Wprimes)){
          grid.mult <- Wprimes[[ii]]$grid * num
          
        }
      }
    }
    
    if(length(num) > 1){
      
      if(length(Wprimes) == length(num)){
        for(ii in 1:length(Wprimes)){
          grid.mult <- Wprimes[[ii]]$grid * num[[ii]]
          
        }
      }else{stop("numeric vector does not match number of landscapes")}
    }
    
  } else{
    
    grids  <- lapply(Wprimes, FUN = function(X) X$grid)
    
    #check grid points match
    for(i in 1:length(grids)){
      
      if(!all(grids[[1]][,1:2] == grids[[i]][,1:2])){
        stop("Grid points do not match between objects 1 and ", i)
      }
    }
    
    grid.mult <- Reduce("*", grids)
    
    
  }
  
  
  
  
  class(grid.mult) <- "combined.surface"
  attr(grid.mult, "parents") <-  names(lscps)
  attr(grid.mult, "operation") <- "mult"
  attr(grid.mult, "mult vector") <-  num
  
  return(grid.mult)
  
}

mult <- mult_lscps(lscps, num = 2)

attributes(mult)





