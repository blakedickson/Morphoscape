

#' Plot a performance surface or adaptive landscape onto a tree
#'
#' @param tree a phylogenetic tree
#' @param tip.scores a matrix containing pc.scores for each tipt taxa
#' @param node.scores a matrix containing pc. scores for each node
#' @param landscape a landscape object from fnc.surface or adap.surface
#' @param palette.cols a color palette
#' @param plot Logical. Plot trait mapped tree. Defaults to FALSE
#' @param palette a palette function
#' @param binary Logical. plot transition as a binary or continuous trait.
#'   Defaults to FALSE
#'
#' @return Returns a list contianing tip and node scores, predicted heights on
#'  the landscape and a cont.mapped tree object
#' @export
#'
#' @examples X
surfaceAlongTree <- function(tree, tip.scores, node.scores = NULL, landscape,
                             palette.cols = NULL, plot = T, palette = NULL, binary=F){
    ### landscpe can either be a functio0nal landscape, adaptive landsape or a dataframe with XYZ coordinates
    ### NOTE NEED TO ADD FUNCTIONALITY FOR ANY SURFACE KIND MENTIONED ABOVE


    # if (attributes(landscape)$class=="surface"){
    surface <- landscape
    # } else{
    #   stop("landscape is not of class 'surface'")
    # }

    if (!isTRUE(row.names(tip.scores)%in%tree$tip.label)) {
        cat("gpa and tree do not match, attempting to match")
        tip.scores <-tip.scores[match(tree$tip.label, row.names(tip.scores)), ]
    }

    if (is.null(node.scores)){
        node.scores <- anc_scores(tip.scores, tree)
    }

    tip.pred <- predict_surf(surface = surface, Zprime = tip.scores)$pred
    names(tip.pred) <- row.names(tip.scores)
    node.pred <- predict_surf(surface = surface, Zprime = node.scores)$pred
    names(node.pred) <- row.names(node.scores)



    levels = pretty(range(surface$z), 1001)
    ind <- which(levels > range(tip.pred)[1] & levels < range(tip.pred)[2])

    if (is.null(palette)){
        palette <- viridis::viridis
    }

    if (is.null(palette.cols)){
        palette.cols <- palette(n =1001,
                                begin = min(ind)/length(levels),
                                end = max(ind)/length(levels))

        names(palette.cols)<- 0:1000
    }

    cont <- contMap(tree = tree, x = tip.pred, method="user",
                    anc.states = node.pred,plot=F)
    cont$cols <- palette.cols


    if (plot){
        par(mfrow=c(1,1))
        plot.contMap(cont,lwd=10)
    }



    return(invisible(list(scores = tip.scores, node.scores = node.scores,
                          pred.scores = list(tip.pred = tip.pred, node.pred = node.pred),
                          cont = cont)))
}


#' Compute node pc scores using ancestral state reconstruction
#'
#' @param scores pc scores for tip taxa
#' @param tree a tree object
#' @param drop logical. automatically drop missing tip taxa from tree. defaults to FALSE
#'
#' @return returns node pc scores
#' @export
#'
#' @examples X
anc_scores <- function(scores, tree, drop = F) {

    if (!isTRUE(tree$tip.label %in% row.names(scores))) {
        print("scores and tree names not ordered, attempting to match")

        if (anyNA(match(tree$tip.label, row.names(scores)))) {
            drop_tip <- tree$tip.label[which(is.na(match(tree$tip.label, row.names(scores))))]

            if (drop) {
                tree <- drop.tip(tree, drop_tip)

            } else {
                print("cannot match taxa: ")
                print(paste(drop_tip, sep = "\n"))

                stop(print("Error cannot match taxa"))
            }
        }

        scores <- scores[match(tree$tip.label, row.names(scores)), ]
    }

    anc.scores <- apply(X = scores, MARGIN = 2, FUN = fastAnc, tree = tree)

    return(anc.scores)
}
