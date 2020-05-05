#' Plot a heatmap of functional data.

#'
#' @param x,y,z x can be an xyz dataframe (such as from fnc.df),
#'   or the user can provide separate xyz vectors
#' @param ... parameters to pass onto image()
#'
#' @examples X
plot.image <- function(x, y, z, ...){ #plots a heatmap of functional data. x can be an xyz dataframe (such as from fnc.df), or the user can provide separate xyz vectors
    if (is.vector(x)){
        X <- data.frame(x,y,z)
    } else{
        X <- x
    }

    image(x = sort(unique(X[,1])), y = sort(unique(X[,2])),
          z = matrix(X[,3], nrow = length(unique(X[,1])),byrow=F),
          ...)

}

# plot.best <- function(pc.scores, weights, mean, length, mfrow = c(1, 1), palette = matlab.like, Fn, xmar = xmar, ymar = ymar, pt.col) {
#
#     par(mfrow = mfrow)
#
#     for (i in 1:length) {
#         wn = weights[i, ]
#
#         surf <- adap.surf(Fn, wn, xmar = xmar, ymar = ymar, n = 50)
#         MAIN <- paste(paste(names(wn), wn, sep = "=", collapse = " "), sep = "")
#
#         nlevels = 40
#         levels = pretty(zlim, nlevels)
#
#         plot.surf(surf = surf$surface, pc.scores = pc.scores, nlevels = 40, pt.col = pt.col, main = MAIN, palette = viridis, xmar = xmar, ymar = ymar)
#
#         points(x = mean[1], y = mean[2], bg = "grey", pch = 21, cex = 3)
#
#
#     }
# }

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
plot_surf <- function(surf, method = "poly", contour = T, contour.lines=NULL, nlevels=40,points=NULL,tree = NULL, axes=T,
                      node.points = NULL, max.point=T, palette=viridis, box=F,
                      pch = 21, pt.col= NULL, ...){

    levels = pretty(range(surf$z), nlevels)
    if(is.function(palette)){
        palette.col <- palette(length(levels))

    }else{
        palette.col <- palette
    }




    if (method == "kriging"){

        Kr <- kriging(x = surf$x, y = surf$y, response = surf$z, pixels = 200)
        image(Kr,
              col = palette.col,
              xlim = extendrange( surf$x),
              ylim = extendrange( surf$y),
              axes = axes,
              ...)
        points(x = x, y = y, cex = (Csize/max(Csize))+1 ,
               pch = pch, bg = pt.col)

    }
    if (method == "poly"){

        plot(surf$x,surf$y, asp=1,type="n",axes= F,...)

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
            axis(side=1, at = pretty(trunc(range(surf$x)*10,digits = 1)/10), pos = min(surf$y))
            axis(side=2, at = pretty(trunc(range(surf$y)*10,digits = 1)/10), pos = min(surf$x))


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

plot.adpt <- function(x, ...){
    
}


