


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




#' Probability of belonging to a groups adaptive peak
#'
#' @param surf a surface object from and adaptive landscape
#' @param train pc scores from test data. Usually the same data used to optimize the landscape
#' @param test pc scores for data to test
#' @param n number of rnorm samples
#' @param plot plot z-height distributions for training and test data. 
#'
#' @return a list with z-height probabilities and the 95% z-height CI for the landscape
#' @export
#'
#' @examples
prob_peak <- function(surf, train, test, n = 1000, plot = F){
  zscores <- predict_surf(surf, train)
  
  zdist <- rnorm(n = n*2, mean = max(zscores$pred), sd = sd(zscores$pred))
  zdist <- zdist[zdist < max(zscores$pred)]
  which(zdist < max(zscores$pred))
  
  
  zpred <- predict_surf(surf, test)
  p <- ecdf(zdist)(zpred$pred)
  p95 <- quantile(zdist, 0.05)
  
  if(plot){
    par(mfrow = c(1,2))
    hist(zscores$pred, breaks= 10, main = "sample dist")
    hist(zdist, breaks = 50,xlim = c(min(zpred$pred), max(zdist)), main = "resample dist")
    arrows(zpred$pred, length(zdist), zpred$pred, 0, col = "red")
    arrows(p95, length(zdist), p95, 0, col = "blue")
  }
  probs <- data.frame("z height" = zpred$pred, "p" = p, "n" = length(zdist))
  row.names(probs) <- row.names(test)
  
  return(list(probs, p95))
  
}
