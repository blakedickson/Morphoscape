


#' Significance test between group weights
#'
#' @param grpa,grpb Weights dataframes generated from search.w.exhaustive()  
#' @param percentile Numeric. Determines the top percentile of group weights. Defaults to 0.01
#' @author Katrina Jones
#' @return
#' @export
#'
#' @examples X
lands.grp.test <- function(grpa, grpb, percentile = 0.01){
  #get top 1% based on likelihood
  besta <- grpa[order(grpa$lik,decreasing = T), ][1:round(nrow(grpa)
                                                         * percentile), ]
  bestb <- grpb[order(grpb$lik,decreasing = T), ][1:round(nrow(grpb)
                                                         * percentile), ]
  #Check for matching models
  m <- match(besta$X, bestb$X)
  n.match <- length(m[!is.na(m)])
  p.match <- n.match / length(m)
  return(list(n.match = n.match, p.val = p.match, 
              matching = besta[na.omit(m), ]))
}


#groups list of weighting files
#' Tests for pairwise signifince between mulitple group weights
#'
#' @param groups A list of named group weights from search.w.exhaustive() 
#' @param percentile Numeric. Determines the top percentile of group weights. Defaults to 0.01
#' @author Katrina Jones
#' @return
#' @export
#'
#' @examples X
multi.lands.grp.test<-function(groups, percentile=0.01){
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
    t <- lands.grp.test(grpa, grpb, percentile=0.01)
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

# #Example code
# wts<-list()
# for(i in 1:length(ls)){
#   dat<-read.csv(paste0(result.dir,ls[i]))
#   wts[[i]]<-dat
# }
# names(wts)<-grnames
# out<-multi.lands.grp.test(wts)
# out