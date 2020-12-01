#' Plot a heatmap of functional data.

#'
#' @param x,y,z x can be an xyz dataframe (such as from fnc.df),
#'   or the user can provide separate xyz vectors
#' @param ... parameters to pass onto image()
#'
#' @examples X
plot_image <- function(x, y, z, ...){ #plots a heatmap of functional data. x can be an xyz dataframe (such as from fnc.df), or the user can provide separate xyz vectors
    if (is.vector(x)){
        X <- data.frame(x,y,z)
    } else{
        X <- x
    }

    image(x = sort(unique(X[,1])), y = sort(unique(X[,2])),
          z = matrix(X[,3], nrow = length(unique(X[,1])),byrow=F),
          ...)

}

# palette can be either a color palette function as in virirdis(), or a color ramp of vectors

#' General plotting function to plot surfaces. Can be used to plot fnc.surfaces,
#'    adapt.surfaces or just a morpohpscae
#'
#' @param surf A $surface object from fnc.surface or adap.surface.
#' @param method "can be "poly" or "kriging" Plot surface as a least-squares polynomial fit, or as a kriging surface
#' @param contour logical. Plot coloured contours onto morphospace.
#'   Defaults to TRUE
#' @param contour.lines Optional. Colour to set contour lines. Defaults to NULL.
#' @param nlevels Number of colour levels to plot contours
#' @param points Optional. Provide points to plot on top of surface
#' @param tree Optional. Plot a tree on top of surface
#' @param node.points points for plotting node. Required if tree is provided
#' @param max.point Optional logical. Plot highest point on surface. Defaults to
#'    TRUE
#' @param palette Optional colour palette. Defaults to viridis colour palette.
#' @param box Logical. Plot box around surface
#' @param axes Logical. Plot axis ticks. Defaults to TRUE
#' @param pch set point character. Defaults to 21
#' @param pt.col set point colour.
#' @param ... other parameters to pass onto plot()
#'
#' @export
#' 
#' @examples X
plot_surf <- function(surf, method = "poly", contour = T, contour.lines=NULL, nlevels=40, points=NULL,tree = NULL, axes=T,
                      node.points = NULL, max.point=T, palette=viridis, box=T, xlab = "", ylab = "",
                      pch = 21, pt.col= NULL, ...){

    if(class(surf) == "Fnc.surf"){
      
      surf <- surf$surface
    }
  
    if(class(surf) == "adap.lscp"){
    
      surf <- surf$surface
    }
  
  
    levels = pretty(range(surf$z), nlevels)
    # if(palette==NULL){
    #   palette <- viridis::viridis  
    # } 
    # 
    if(is.function(palette)){
        palette.col <- palette(length(levels))

    } else{
        palette.col <- palette
    }
    
    if (method == "kriging"){

        Kr <- kriging::kriging(x = surf$x, y = surf$y, response = surf$z, pixels = 200)
        image(Kr,
              col = palette.col,
              xlim = extendrange( surf$x),
              ylim = extendrange( surf$y),
              axes = axes,
              ...)
        points(x = x, y = y, cex = (Csize/max(Csize))+1 ,
               pch = pch, ...)

    }
    if (method == "poly"){

      
      
        plot(surf$x,surf$y, asp=1,type="n", xlab = "", ylab = "", axes= F, ...)

        if(contour){
            .filled.contour(x=surf$x, y=surf$y, z=surf$z,
                            levels=levels,
                            col=palette.col
            )

        }

        if(!is.null(contour.lines)){
            contour(x=surf$x, y=surf$y, z=surf$z,
                    levels=levels, col = contour.lines,
                    add = T, drawlabels = F )

        }

        if (axes){
            axis(side=1, at = pretty(trunc(range(surf$x)*20,digits = 1)/20), pos = min(surf$y))
            axis(side=2, at = pretty(trunc(range(surf$y)*20,digits = 1)/20), pos = min(surf$x))


        }

        if (box){
            axis(side=1, at = range(surf$x), labels = F, lwd.ticks = 0, pos = min(surf$y))
            axis(side=2, at = range(surf$y), labels = F, lwd.ticks = 0, pos = min(surf$x))
            axis(side=3, at = range(surf$x), labels = F, lwd.ticks = 0, pos = max(surf$y))
            axis(side=4, at = range(surf$y), labels = F, lwd.ticks = 0, pos = max(surf$x))
        }


        if(!is.null(tree)){
            tree.points <- rbind(points[match(tree$tip.label,row.names(points)), ],
                                 node.points)
            for (i in 1:nrow(tree$edge)) {
                lines(tree.points[(tree$edge[i, ]), 1],
                      tree.points[(tree$edge[i, ]), 2], type = "l", pch = 21,
                      col = "grey50", lwd = 2)
            }
            points(node.points,bg="grey",pch=21)

        }

        if(!is.null(points)){
            points(points, pch = pch ,bg = pt.col,...)

        }

        if(max.point){
            x <- surf$x[which(surf$z==max(surf$z),arr.ind = T)[1]]
            y <- surf$y[which(surf$z==max(surf$z),arr.ind = T)[2]]
            points(x,y, cex=2, bg="black",pch=21)

        }
    }
}

#' Plot multiple surfaces
#'
#' @param multi.surf A multi-surface pobject from multi.fnc.surface, or a list of adaptive surfaces 
#' @param ... optional parameters to pass onti plot_surf and par
#'
#' @return
#' @export
#'
#' @examples X
plot_multisurf <- function(multi.surf, main = NULL, ...){
  
  args <- list(multi.surf, main = NULL, ...)
  
  if(is.null(main)){
    main = names(multi.surf)
  }
  
  if(class(multi.surf) == "multi.Fnc.surf"){
    
    # lapply(multi.surf, FUN = function(X,...) plot_surf(surf = X$surface, ...), ...)
    for(i in 1:length(multi.surf)){
      
      plot_surf(multi.surf[[i]]$surf, main = main[i], ...)
      
    }
    
  }
  
}

# plot_adpt <- function(X, ...){
#     
# }
# 
# 
# plot_trans <- function(X, ...){
#     
# }
# 
# plot_pareto <- function(X, ...){
#     
#     
# }

