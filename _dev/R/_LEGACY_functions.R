
# LEGACY FUNCTIONS. MANY OF THESE ARE PERMANENTLY DEPRICATED. SOME WILL BE REVAMPED FOR FUTURE RELEASES


# THIS FUNCTION IS PERMANENTLY DEPRICATED

#' Calculate combined surface calculate W on the adaptive landscape
#'  W = w1*F1 + w2*F2 (Polly et al, 2016)
#'
#' @param Fn A multi.surf object from multi.poly.surface
#' @param wn A set of performance weights. Can be user derived or generated
#'   using search.w.exhaustive
#' @param xmar,ymar A vector of length 2 defining the X and Y limits
#' @param n The number of grid points to subset along each axis
#' @param ... Parameters to pass onto nested functions
#'
#' @return A polynomial function and surface defining the summed adaptive
#'   landscape
#' @export
#'
#' @examples X
# adap.surf <- function(Fn, wn, xmar, ymar, n, ...) {
# 
#         Fn.tmp <- list()
#         for (i in 1:length(Fn)){
#             Fn.tmp[[i]] <-Fn[[i]]
# 
#         }
#         names(Fn.tmp) <- names(Fn)
#         Fn <- Fn.tmp
# 
# 
#     dx <- (xmar[2] - xmar[1]) / n
#     dy <- (ymar[2] - ymar[1]) / n
#     x  <- seq(xmar[1], xmar[2], dx)
#     y  <- seq(ymar[1], ymar[2], dy)
#     Zprime.grid <- expand.grid(x, y)
#     zraw <- matrix(apply(Zprime.grid, 1, FUN=W, wn, Fn),
#                    nrow=length(x), ncol=length(y), byrow= F)
# 
#     z <- (zraw - min(zraw))
#     z <- z / max(z)
# 
#     surface <- list(x = x, y = y, z = z)
#     # attr(surface, "class") <- "surf"
#     
#     adap.surface <- list(surface = surface,
#                          zraw = zraw,model=list(Fn=Fn,wn=wn))
#     # attr(adap.surface, "class") <- "adap.lscp"
# 
#     return(adap.surface)
# }

# 

# THIS FUNCTION IS CURRENTLY DEPRICATED IN THIS VERSION

#' Produces a 3D polynomial surface of a single functional dataframe.
#'     Useful for testing and checking data
#'
#' @param X An x,y,z funtional dataframe produced by fnc_dataframe
#' @param npoly Numeric. Determines the degree of polynomial to apply to the
#'     surface. Usually 3rd order polynomials produce the best fit
#' @param npoints Numeric. Optional argument to up or downsample the number x,y
#'     grid points. Defaults to the number of observations in X
#' @param fnc.name Optional string to label the functional surface
#' @param plot Logical. Plot surfaces. Defaults to true
#' @param pad Add padding to range data. Defaults to adding 0.2 on all range margins
#' @param npoly Number of polynomials
#' @param ... optional paramaters to pass onto plot
#' @return Returns a list of polynomial surface objects:
#'     poly: the polynomial model applied to X
#'     summary: the polynomial model summary
#'     surface: the polynomial surface fit to X
#' @export
#'
#' @examples X
# poly.surface <- function(X, method = "poly", npoints = NULL, plot = F, pad = 1.2, fnc.name = NULL, range = NULL, npoly = 3,...){
#     X <- as.matrix(X)
# 
#     X <- na.omit(X)
# 
#     x <- X[,1]
#     y <- X[,2]
#     z <- X[,3]
# 
# 
#     if (is.null(range)){
#         range <- rbind(range(x)*pad,
#                        range(y)*pad)
# 
#     }
# 
#     if (is.null(npoints)){
#         npoints = length(x)
#     }
# 
#     if (!is.null(fnc.name)){
#         print(fnc.name)
#     }
# 
#     if (method == "poly"){
#         npoly = npoly
#         poly <- spatial::surf.ls(np = npoly, x = x, y = y, z = z) #fit polynomial least squares trend surface
#         summary(poly)
#         poly.surf<-spatial::trmat(poly, range[1,1], range[1,2],
#                          range[2,1], range[2,2], npoints) # evaluate grid points over surface
#         poly.surf$z <- scale.z(poly.surf$z)
#         # attr(poly.surf, "class") <- "surf"
#         fn.surf <- list(poly = poly, surface = poly.surf)
#         # attr(fn.surf,"class") <- "Fnc.surf"
# 
#         return(fn.surf)
#     }
# 
# 
#     ### ADD CLASS AND S3 METHOPDS FOR PLOTTING
#     if (method == "kriging"){
#         Kr <- kriging(x = x, y = y, response = z)
# 
#         return(Kr)
# 
#     }
#     return(list(poly = poly, surface = poly.surf))
# }





# THIS FUNCTION IS PERMANENTLY DEPRICATED

#' Produces a list of polynomial surface fits from multiple functional
#'     dataframes. Is essentailly an apply wrapper for poly.surface and krige_surface
#'
#' @param X A data table of XY coordinates and Z trait values, or a list of x,y,z functonal dataframes from 'fnc.dataframe'
#' @param method 
#' @param ... Paramaters to pass onto poly.surface and plot         
#'
#' @return Returns a multi.poly.surface object. A list containing N number of
#'     functional surfaces. See poly.surface for details
#' @export
#'
#' @examples X
# multi.surface <- function(X, method = c("polynomial", "kriging"), par = par(), ...){
# 
#     if( class (X) != "fnc.df"){
#         X <- fnc.dataframe(X)
#     }
#   
#     multi.surf <- list()
#   
#     if(method == "polynomial"){
#       for(l in 1:length(X)){
#         multi.surf[[l]] <- poly.surface(X[[l]],
#                                         fnc.name = names(X)[l], ...)
#       }  
#       
#     }
#     if(method == "kriging"){
#     
#     }
#       
#     names(multi.surf) = names(X)
#     # attr(multi.surf,"class") <- "multi.Fnc.surf"
#     return(multi.surf)
# }








#' ## W calculates the height on the combined adaptive landscape at any point Zprime, given your functional landscapes (Fn) and a weight (w) Zprime = a
#' ## corrdinate in morphospace (Pc1 and PC2 score) Fn = a list of functional surfaces wn = a vector of weights corressponding to each functional surface
#' 
#' Fnc.surf <- function(perf.surface, Zprime) {
#'   Zprime <- matrix(Zprime, ncol = 2)
#'   pred <- predict(perf.surface$poly, x = Zprime[,1], y = Zprime[,2])
#'   return(pred)
#'   
#' }
#' 
#' 
#' W <- function(Zprime, wn, Fn) {
#' 
#'   wbar <- sum(sapply(Fn, FUN = Fnc.surf, Zprime = Zprime) * wn)
#' 
#'   return(wbar)
#' }
#' 
#' 
#' scale.z <- function(Z) {
#' 
#'   Z <- Z - min(Z)
#'   Z <- Z/max(Z)
#'   return(Z)
#' }
#' 
#' 
#' parti <- function(n, k) {
#'   if (n < 0) {
#'     message("error: n<0")
#'     return(NA)
#'   }
#'   if (k == 1)
#'     return(matrix(n, 1, 1))
#'   M <- cbind(parti(n, k - 1), 0)
#'   if (n > 0)
#'     for (i in (1:n)) M <- rbind(M, cbind(parti(n - i, k - 1), i))
#'   M
#'   
#' }
#' 
#' 
#' max_Wprime <- function(seed, Fn, wn, xmar, ymar, ...) {
#' 
#' 
#'   if (length(Fn[[1]]) == 3) {
#'     Fn.tmp <- list()
#'     for (i in 1:length(Fn)) {
#'       Fn.tmp[[i]] <- Fn[[i]]$poly
#' 
#'     }
#'     names(Fn.tmp) <- names(Fn)
#'     Fn <- Fn.tmp
#'     
#' 
#'   }
#' 
#' 
#'   # Wprime <- optim(par = seed, fn = W, lower = c(xmar[1], ymar[1]), upper = c(xmar[2], ymar[2]), method = "L-BFGS-B", control = list(fnscale = -1),
#'   #                 wn = wn, Fn = Fn)
#' 
#'   Wprime = max(Fn[[]])
#'   
#'   return(Wprime)
#' }
#' 
#' 
#' lik_Zprime <- function(wn, Zprime, Fn, xmar = xmar, ymar = ymar, method = c("mean", "sum"), optimum = c("absolute", "relative", "dist")) {
#' 
#'   if (method == "sum") {
#'     Wprime <- mean(sapply(Zprime, FUN = W, Fn = Fn, wn = wn))
#' 
#'   } else {
#'     if (is.matrix(Zprime)) {
#'       Zprime <- colMeans(Zprime)
#'     }
#'     Wprime <- W(Zprime = Zprime, Fn = Fn, wn = wn)
#'   }
#' 
#'   # max <- max_Wprime(seed = Zprime, Fn, wn, xmar = xmar, ymar = ymar) # promblematic for autokrige method
#' 
#'   lik <- log((Wprime))
#'   # dis <- as.vector(1 - mean(dist(rbind(Zprime, max$par))))
#'   z <- (Wprime)
#' 
#'   # X <- c(Wprime, lik, dis)
#'   X <- c(Wprime, lik)
#'   # names(X) <- c("z", "lik", "dis")
#'   names(X) <- c("z", "lik")
#'   return(X)
#' }
#' 
#' 
#' 
#' predict_surf <- function(surface, Zprime) {
#'   X <- expand.grid(surface$x, surface$y)
#'   colnames(X) <- c("x", "y")
#'   z <- as.vector(surface$z)
#'   XZ <- data.frame(X, z)
#'   ls <- surf.ls(np = 3, XZ)
#'   pred <- predict(object = ls, x = as.vector(Zprime[, 1]), y = as.vector(Zprime[, 2]))
#'   return(data.frame(Zprime, pred))
#' }
#' 
#' 
#' 
#' euc.dists <- function(X1, X2) {
#'   dists <- matrix(NA, nrow = nrow(X1), ncol = nrow(X2))
#'   for (i in 1:nrow(X1)) {
#'     tmp <- vector()
#'     for (ii in 1:nrow(X2)) {
#' 
#'       tmp[ii] <- dist(rbind(X1[i, ], X2[ii, ]))
#'       # tmp[ii] <- sqrt( (X1[i,1] - X2[ii,1])^2 + (X1[i,2] - X2[ii,2])^2 )
#' 
#'     }
#'     dists[i, ] <- tmp
#' 
#'   }
#'   min.dists <- apply(dists, MARGIN = 1, min)
#'   mean.dist <- mean(min.dists)
#'   return(list(dists = dists, min.distance = min.dists, mean.distance = mean.dist))
#' }
#' 
#' 
#' 
#' 
#' 
#' # THIS FUNCTION IS DEPRICATED BY calc_all_lscps()
#' 
#' # method can be either 'sum' or 'mean'. Sum will caculate w for all points given and sum them, mean with calculate only the mean of the points given.
#' # default is 'mean' which is faster optimum can be either 'absolute' or 'relative'.  absolute calculates the absolute height on the landscape, where
#' # relative compares it to the wmax, the heightest point on the landscape.  'Relative' can sometimes give odd results and should be used with caution
#' 
#' #' Calculate the optimum weights for each trait given a position, or group in
#' #' morphospace
#' #'
#' #' @param Zprime Numeric vector of length=2, or Nx2 matrix containing x,y
#' #'     coordiates to optimize for
#' #' @param Fn A surface list containing traits to model for
#' #' @param weights.df An optional matrix containing all possible combinations of
#' #'      wn given n number of traits.
#' #' @param step Optional numeric. The degree by which to vary wn by. Required if
#' #'     weights.df is not provided. Smaller step sizes require exponentially more
#' #'      computation time.
#' #' @param xmar,ymar Numeric vector containing min/max margins for morphospace
#' #' @param Cluster logical. Split computation across multiple machines.
#' #'     Currently not working
#' #' @param method Method by which to summarize optimization results when multiple
#' #'      Zprime coordintes are provided. Both produce similar results, but have
#' #'       not been stress tested.
#' #' @param optimum Criteria to optimize for and sort results by. Currently only
#' #'     "absolute" height on the landscape is supported, though all optimization
#' #'     metrics are returned. See details.
#' #'
#' #' @return A matrix containing three optimization metrics for all combinations
#' #'     of weights as provided by weights.df, or generated by 'step'.
#' #'     Automatically sorted by 'absolute'.
#' #' @export
#' #' @details
#' #'
#' #' @examples X
#' # search.w.exhaustive <- function(step =NULL, Zprime, Fn, Cluster = F,
#' #                                 method = c("mean","sum"),
#' #                                 optimum = c("absolute"),
#' #                                 xmar=xmar, ymar=ymar,weights.df = NULL){
#' # 
#' # 
#' #         Zprime <- matrix(Zprime[1:2], ncol = 2)
#' #         colnames(Zprime) <- c("x","y")
#' #         
#' # 
#' # 
#' #     if(length(Fn[[1]]) == 3) {
#' #         Fn.tmp <- list()
#' #         for (i in 1:length(Fn)){
#' #             Fn.tmp[[i]] <-Fn[[i]]$poly
#' # 
#' #         }
#' #         names(Fn.tmp) <- names(Fn)
#' #         Fn <- Fn.tmp
#' # 
#' #     }
#' # 
#' #     if(is.null(weights.df)){
#' #         # if(is.null(weights.df)){
#' #         #     stop("no step size provided. Please provide either a step size or weights dataframe")
#' #         # }
#' #         weights.df <- generate_weights(step,nvar=length(Fn))
#' #         colnames(weights.df) <- names(Fn)
#' # 
#' #     }
#' #     # Cluster mode is not currently working | must troubleshoot
#' #     # if (Cluster){
#' #     #     no_cores <- detectCores()
#' #     #     cl <- makeCluster(no_cores)
#' #     #     clusterExport(cl, varlist=c("max_Wprime","DEoptim",
#' #     #                                 "Zprime","Fnc.surf","W","lik_Zprime",
#' #     #                                 "Fn","xmar","ymar"),
#' #     #                   envir=environment() )
#' #     # 
#' #     #     lik <- parApply(cl = cl, X = weights.df, MARGIN = 1, FUN=lik_Zprime,
#' #     #                     Zprime=Zprime, Fn = Fn,
#' #     #                     xmar=xmar, ymar=ymar)
#' #     #     stopCluster(cl)
#' #     # 
#' #     # } else{
#' # 
#' #         lik <- apply(X = weights.df, MARGIN = 1, FUN=lik_Zprime,
#' #                      Zprime=Zprime, Fn = Fn,
#' #                      xmar=xmar, ymar=ymar,
#' #                      method = method,
#' #                      optimum = optimum)
#' # 
#' #     # }
#' # 
#' #     colnames(weights.df)[1:length(Fn)] <- names(Fn)
#' #     weights.df <- cbind(weights.df, t(lik))
#' # 
#' #     print("Done!")
#' #     return(weights.df)
#' # }
#' 
#' 
#' 
#' # THIS FUNCTION IS CURRENTLY NON-FUNCTIONAL IN THIS VERSION 
#' 
#' #' Calculate pareto front between two landscapes
#' #'
#' #' @param surf.1,surf.2 surface objects
#' #' @param plot logical, plot pareto front
#' #' @param a sequence from 1-0 to calculate the pareto transtion
#' #' @param ... additional parameters for plot.surf
#' #' @param smooth optional c("spline", "kernel"), determines which smoothing
#' #'   method to use. The "spline" method will fail if mulitple values of Y exist
#' #'   for a given X, in which case use "kernel"
#' #'
#' #' @return a list containing the pareto transition
#' #' @export
#' #'
#' #' @examples X
#' # pareto <- function(surf.1,surf.2,plot=F,a=NULL, smooth = c("kernel"), ...){
#' # 
#' #     if (is.null(a)){
#' #         a <- seq(1,0, by= -0.025)
#' #     }
#' #     b <- 1-a
#' #     comb <- surf.1
#' # 
#' #     z <- vector()
#' #     x <- vector()
#' #     y <- vector()
#' # 
#' #     i=1
#' #     for(i in 1:length(a)){
#' #         comb$z <- rep(NA,length(surf.1$z))
#' #         comb$z <- surf.1$z*a[i] + surf.2$z*b[i]
#' #         if (plot){
#' #           dev.off()
#' #           plot_surf(comb,main="Combined",...)
#' # 
#' #         }
#' # 
#' # 
#' #         z[i] <- max(comb$z)
#' #         x[i] <- comb$x[which(comb$z==max(comb$z),arr.ind = T)[1]]
#' #         y[i] <- comb$y[which(comb$z==max(comb$z),arr.ind = T)[2]]
#' # 
#' #     }
#' # 
#' # 
#' #     surf.1$max$x <- surf.1$x[which(surf.1$z==max(surf.1$z),arr.ind = T)[1]]
#' #     surf.1$max$y <- surf.1$y[which(surf.1$z==max(surf.1$z),arr.ind = T)[2]]
#' #     surf.1$max$z <- max(surf.1$z)
#' #     surf.1.max <- data.frame(surf.1$max)
#' # 
#' #     surf.2$max$x <- surf.2$x[which(surf.2$z==max(surf.2$z),arr.ind = T)[1]]
#' #     surf.2$max$y <- surf.2$y[which(surf.2$z==max(surf.2$z),arr.ind = T)[2]]
#' #     surf.2$max$z <- max(surf.2$z)
#' #     surf.2.max <- data.frame(surf.2$max)
#' #     pareto<-data.frame(x,y,z)
#' #     
#' #     if (smooth == "kernel"){
#' #         pareto.line <- smoothr::smooth_ksmooth(as.matrix(pareto[,1:2]),
#' #                                                smoothness=10)
#' #     }
#' #     
#' #     
#' #     
#' #     return(list(pareto=pareto, pareto.line = pareto.line, 
#' #                 surf.1.max=surf.1.max,surf.2.max=surf.2.max))
#' # }
#' 
#' 
#' 
#' # THIS FUNCTION IS CURRENTLY NON-FUNCTIONAL IN THIS VERSION 
#' 
#' 
#' #' Sum any number of adaptive landscapes
#' #'
#' #' @param landscapes a list of adaptive lanscapes
#' #'
#' #' @return Returns a combined adpative landscape
#' #' @export
#' #'
#' #' @examples X
#' # sum_surface <- function(landscapes) {
#' #     L <- list()
#' #     for (l in 1:length(landscapes)) {
#' #         L[[l]] <- landscapes[[l]]$surface$z
#' #     }
#' #     lscp.sum <- Reduce("+", L)
#' #     L <- list(x = landscapes[[1]]$surface$x, y = landscapes[[1]]$surface$y, z = lscp.sum)
#' #     class(L) <- "surf"
#' #     return(L)
#' # }
#' 
#' # THIS FUNCTION IS CURRENTLY NON-FUNCTIONAL IN THIS VERSION 
#' 
#' 
#' #' Calculate a transition landscape between two landscapes
#' #'
#' #' @param X,Y A $surface onject
#' #' @param binary Logical. Calcuate as a binary transition (STILL IN
#' #'   DEVELOPMENT). Defaults to FALSE
#' #'
#' #' @return Returns a $surface object
#' #' @export
#' #'
#' #' @examples X
#' # trans.surface <- function(X, Y, binary = F) {
#' #     L <- X
#' #     L$surface$z <- (X$surface$z + 1)/(Y$surface$z + 1)
#' # 
#' #     if (binary) {
#' #         bn <- L$surface$z > 1
#' #         L$surface$z[bn] <- 1
#' #     }
#' #     class(L) <- "surf"
#' #     return(L)
#' # }
#' 
#' 
#' # # THIS FUNCTION IS CURRENTLY NON-FUNCTIONAL IN THIS VERSION 
#' # 
#' # sub_surface <- function(surf.1, surf.2 ){
#' #     L <- X
#' #     L$surface$z <- (X$surface$z + 1)/(Y$surface$z + 1)
#' #     
#' #     if (binary) {
#' #         bn <- L$surface$z > 1
#' #         L$surface$z[bn] <- 1
#' #     }
#' #     class(L) <- "surf"
#' #     return(L)
#' # 
#' # }
#' 
#' 
#' 
#' 
#' #' Splits a weights dataframe into a number of jobs to be computed separately
#' #'
#' #' @param weights.df A dataframe of weight combinations to split.
#' #'   Can be generated from generate_weights
#' #' @param njobs The number of jobs to split into
#' #' @param out.dir The directory to save jobs to
#' #' @param overwrite A logical scalar. Should the output folder be overwitten?
#' #'   Defaults to FALSE
#' #'
#' #' @return Writes njobs number of job files to be computed separatley
#' #' @export
#' #'
#' #' @examples X X
#' split_jobs <- function(weights, njobs, dir = NULL){
#'   
#'   to <- floor(seq(0, nrow(weights), length.out = njobs+1))
#'   
#'   from <- to+1
#'   from <- from[-length(from)]
#'   to <- to[-1]
#'   
#'   
#'   if(is.null(dir)){
#'     dir = "./jobs"
#'   }
#'   
#'   job.dir <- paste(dir,"/jobs",sep="")
#'   ans.dir <- paste(dir,"/ans",sep="")
#'   
#'   unlink(job.dir, recursive = T)
#'   unlink(ans.dir, recursive = T)
#'   
#'   dir.create(job.dir, recursive = T)
#'   dir.create(ans.dir, recursive = T)
#'   
#'   for (i in 1:length(from) ) {
#'     weights.df <- weights[from[i]:to[i],]
#'     save(weights.df, file= paste(job.dir,"/job_",i,".Rdata",sep=""))
#'   }
#'   
#' }
#' 
#' 
#' #' Initialize a distributed analysis of trait weights
#' #'
#' #' @param Fn.surfaces A dataframe containing trait performance surfaces
#' #'   generated from multi.fnc.surface
#' #' @param morphospace A pca object defining the morphospace
#' #' @param dir Directory to store cluster analysis objects
#' #' @param weights Optional. A dataframe containing weights from generate_weights
#' #' @param step Optional. If weights is not provided, specify step size
#' #' @param njobs Number of jobs to split into
#' #'
#' #' @return A directory of split jobs
#' #' @export 
#' #'
#' #' @examples X
#' init.QTM.cluster <- function(Fn.surfaces, morphospace, dir, weights = NULL, step = NULL, njobs){
#'   
#'   if(!dir.exists(dir)){
#'     dir.create(dir)
#'   }
#'   
#'   
#'   nvar = length(Fn.surfaces)
#'   
#'   if(is.null(weights)){
#'     if(is.null(step))
#'     {stop (cat("step size is not defined"))
#'     }  else{
#'       
#'       weights <- generate_weights(step = step, verbose = T, nvar= length(Fn.surfaces))
#'     }
#'   }
#'   
#'   num.perm <- nrow(weights)
#'   
#'   xmar<-range(Fn.surfaces[[1]][[1]]$x)
#'   ymar<-range(Fn.surfaces[[1]][[1]]$y)
#'   
#'   split_jobs(weights, njobs, dir)
#'   
#'   save(Fn.surfaces, morphospace, xmar, ymar, num.perm, step, njobs,
#'        file = paste(dir,"/QTMdata.Rdata",sep=""))
#'   
#' }
#' 
#' 
#' 
#' loadRData <- function(fileName){
#'   #loads an RData file, and returns it
#'   load(fileName)
#'   get(ls()[ls() != "fileName"])
#' }
#' 
#' 
#' 
#' #' recombines completed split jobs
#' #'
#' #' @param ans.dir Directory containing compoleted jobs
#' #'
#' #' @return returns a combined matrix of all computed weight combinations
#' #' @export
#' #'
#' #' @examples X
#' reduce_ans <- function(ans.dir, sortby = c("z","lik","dist")){
#'   ls <- list.files(ans.dir)
#'   ans <- NULL
#'   
#'   for (i in 1:length(ls)){
#'     
#'     tmp.ans<- loadRData(file.path(ans.dir, ls[i]))
#'     ans <- rbind(ans,tmp.ans)
#'     
#'   }
#'   
#'   ans <- as.data.frame(ans)
#'   
#'   
#'   ans <- ans[order(ans[,sortby], decreasing = T),]
#'   
#'   return(ans)
#'   
#' }
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' #' Plot a performance surface or adaptive landscape onto a tree
#' #'
#' #' @param tree a phylogenetic tree
#' #' @param tip.scores a matrix containing pc.scores for each tipt taxa
#' #' @param node.scores a matrix containing pc. scores for each node
#' #' @param landscape a landscape object from fnc.surface or adap.surface
#' #' @param palette.cols a color palette
#' #' @param plot Logical. Plot trait mapped tree. Defaults to FALSE
#' #' @param palette a palette function
#' #' @param binary Logical. plot transition as a binary or continuous trait.
#' #'   Defaults to FALSE
#' #' @param ... Optional parameters to pass to plot.contMap
#' #'   
#' #'
#' #' @return Returns a list contianing tip and node scores, predicted heights on
#' #'  the landscape and a cont.mapped tree object
#' #' @export
#' #'
#' #' @examples X
#' surfaceAlongTree <- function(tree, tip.scores, node.scores = NULL, landscape,
#'                              palette.cols = NULL, plot = T, palette = NULL, binary=F, ...){
#'   ### landscpe can either be a functio0nal landscape, adaptive landsape or a dataframe with XYZ coordinates
#'   ### NOTE NEED TO ADD FUNCTIONALITY FOR ANY SURFACE KIND MENTIONED ABOVE
#'   
#'   
#'   # if (attributes(landscape)$class=="surface"){
#'   surface <- landscape
#'   # } else{
#'   #   stop("landscape is not of class 'surface'")
#'   # }
#'   
#'   if (!isTRUE(row.names(tip.scores)%in%tree$tip.label)) {
#'     cat("gpa and tree do not match, attempting to match")
#'     tip.scores <-tip.scores[match(tree$tip.label, row.names(tip.scores)), ]
#'   }
#'   
#'   if (is.null(node.scores)){
#'     node.scores <- anc_scores(tip.scores, tree)
#'   }
#'   
#'   tip.pred <- predict_surf(surface = surface, Zprime = tip.scores)$pred
#'   names(tip.pred) <- row.names(tip.scores)
#'   node.pred <- predict_surf(surface = surface, Zprime = node.scores)$pred
#'   names(node.pred) <- row.names(node.scores)
#'   
#'   
#'   
#'   levels = pretty(range(surface$z), 1001)
#'   ind <- which(levels > range(tip.pred)[1] & levels < range(tip.pred)[2])
#'   
#'   if (is.null(palette)){
#'     palette <- viridis::viridis
#'   }
#'   
#'   if (is.null(palette.cols)){
#'     palette.cols <- palette(n =1001,
#'                             begin = min(ind)/length(levels),
#'                             end = max(ind)/length(levels))
#'     
#'     names(palette.cols)<- 0:1000
#'   }
#'   
#'   cont <- contMap(tree = tree, x = tip.pred, method="user",
#'                   anc.states = node.pred,plot=F)
#'   cont$cols <- palette.cols
#'   
#'   
#'   if (plot){
#'     # par(mfrow=c(1,1))
#'     plot.contMap(cont, ...)
#'   }
#'   
#'   
#'   
#'   return(invisible(list(scores = tip.scores, node.scores = node.scores,
#'                         pred.scores = list(tip.pred = tip.pred, node.pred = node.pred),
#'                         cont = cont)))
#' }
#' 
#' 
#' #' Compute node pc scores using ancestral state reconstruction
#' #'
#' #' @param scores pc scores for tip taxa
#' #' @param tree a tree object
#' #' @param drop logical. automatically drop missing tip taxa from tree. defaults to FALSE
#' #'
#' #' @return returns node pc scores
#' #' @export
#' #'
#' #' @examples X
#' anc_scores <- function(scores, tree, drop = F) {
#'   
#'   if (!isTRUE(tree$tip.label %in% row.names(scores))) {
#'     print("scores and tree names not ordered, attempting to match")
#'     
#'     if (anyNA(match(tree$tip.label, row.names(scores)))) {
#'       drop_tip <- tree$tip.label[which(is.na(match(tree$tip.label, row.names(scores))))]
#'       
#'       if (drop) {
#'         tree <- drop.tip(tree, drop_tip)
#'         
#'       } else {
#'         print("cannot match taxa: ")
#'         print(paste(drop_tip, sep = "\n"))
#'         
#'         stop(print("Error cannot match taxa"))
#'       }
#'     }
#'     
#'     scores <- scores[match(tree$tip.label, row.names(scores)), ]
#'   }
#'   
#'   anc.scores <- apply(X = scores, MARGIN = 2, FUN = fastAnc, tree = tree)
#'   
#'   return(anc.scores)
#' }
#' 
#' 
#' 
#' #' # THIS FUNCTION IS CURRENTLY DEPRICATED IN THIS VERSION. IT WILL BE UPDATED IN LATER VERSIONS
#' #' 
#' #' # palette can be either a color palette function as in virirdis(), or a color ramp of vectors
#' #' 
#' #' #' General plotting function to plot surfaces. Can be used to plot fnc.surfaces,
#' #' #'    adapt.surfaces or just a morpohpscae
#' #' #'
#' #' #' @param surf A $surface object from fnc.surface or adap.surface.
#' #' #' @param method "can be "poly" or "kriging" Plot surface as a least-squares polynomial fit, or as a kriging surface
#' #' #' @param contour logical. Plot coloured contours onto morphospace.
#' #' #'   Defaults to TRUE
#' #' #' @param contour.lines Optional. Colour to set contour lines. Defaults to NULL.
#' #' #' @param nlevels Number of colour levels to plot contours
#' #' #' @param points Optional. Provide points to plot on top of surface
#' #' #' @param tree Optional. Plot a tree on top of surface
#' #' #' @param node.points points for plotting node. Required if tree is provided
#' #' #' @param max.point Optional logical. Plot highest point on surface. Defaults to
#' #' #'    TRUE
#' #' #' @param palette Optional colour palette. Defaults to viridis colour palette.
#' #' #' @param box Logical. Plot box around surface
#' #' #' @param axes Logical. Plot axis ticks. Defaults to TRUE
#' #' #' @param pch set point character. Defaults to 21
#' #' #' @param pt.col set point colour.
#' #' #' @param ... other parameters to pass onto plot()
#' #' #'
#' #' #' @export
#' #' #' 
#' #' #' @examples X
#' #' plot_surf <- function(surf, contours = TRUE, contour.lines=NULL, nlevels=41, points=NULL,tree = NULL, axes=T,
#' #'                       node.points = NULL, max.point=T, palette = NULL, box=T, xlab = "", ylab = "",
#' #'                       pch = 21, grid = F, pt.col= NULL, ...){
#' #'   
#' #'   
#' #'   
#' #'   if(any(class(surf) == "autoKrige")){
#' #'     
#' #'     X <- cbind(surf$krige_output@coords, surf$krige_output$var1.pred)
#' #'     
#' #'     surf <- list(x = X[,1], y = X[,2], z = X[,3]) 
#' #'   } else{
#' #'     
#' #'     surf <- surf$surface
#' #'   }
#' #'   
#' #'   
#' #'   levels = pretty(range(surf$z), nlevels)
#' #'   nlevels = length(levels)
#' #'   if(is.null(palette)){
#' #'     require(viridis)
#' #'     palette <- viridis::viridis(nlevels)
#' #'   }
#' #'   
#' #'   if(is.function(palette)){
#' #'     palette.col <- palette(nlevels)
#' #'     
#' #'   } else{
#' #'     palette.col <- palette
#' #'   }
#' #'   
#' #'   
#' #'   
#' #'   
#' #'   
#' #'   plot(surf$x,surf$y,type="n", xlab = "", ylab = "", axes= F, ...)
#' #'   # plot(surf$x,surf$y, asp=1,type="n", xlab = "", ylab = "", axes= F,
#' #'   #      xlim = range(surf$x), ylim = range(surf$y))
#' #'   
#' #'   
#' #'   if(contours){
#' #'     .filled.contour(x=surf$x, y=surf$y, z=surf$z,
#' #'                     levels=levels,
#' #'                     col=palette.col
#' #'     )
#' #'     
#' #'   }
#' #'   
#' #'   if(!is.null(contour.lines)){
#' #'     contour(x=surf$x, y=surf$y, z=surf$z,
#' #'             levels=levels, col = contour.lines,
#' #'             add = T, drawlabels = F )
#' #'     
#' #'   }
#' #'   
#' #'   if (axes){
#' #'     axis(side=1, at = axisTicks(usr = range(surf$x), log = F), pos = min(surf$y))
#' #'     axis(side=2, at = axisTicks(usr = range(surf$y), log = F), pos = min(surf$x))
#' #'     
#' #'     
#' #'   }
#' #'   
#' #'   if (box){
#' #'     axis(side=1, at = range(surf$x), labels = F, lwd.ticks = 0, pos = min(surf$y))
#' #'     axis(side=2, at = range(surf$y), labels = F, lwd.ticks = 0, pos = min(surf$x))
#' #'     axis(side=3, at = range(surf$x), labels = F, lwd.ticks = 0, pos = max(surf$y))
#' #'     axis(side=4, at = range(surf$y), labels = F, lwd.ticks = 0, pos = max(surf$x))
#' #'   }
#' #'   
#' #'   
#' #'   if(!is.null(tree)){
#' #'     tree.points <- rbind(points[match(tree$tip.label,row.names(points)), ],
#' #'                          node.points)
#' #'     for (i in 1:nrow(tree$edge)) {
#' #'       lines(tree.points[(tree$edge[i, ]), 1],
#' #'             tree.points[(tree$edge[i, ]), 2], type = "l", pch = 21,
#' #'             col = "grey50", lwd = 2)
#' #'     }
#' #'     points(node.points,bg="grey",pch=21)
#' #'     
#' #'   }
#' #'   
#' #'   if(!is.null(points)){
#' #'     points(points, pch = pch ,bg = pt.col,...)
#' #'     
#' #'   }
#' #'   
#' #'   if(max.point){
#' #'     x <- surf$x[which(surf$z==max(surf$z),arr.ind = T)[1]]
#' #'     y <- surf$y[which(surf$z==max(surf$z),arr.ind = T)[2]]
#' #'     points(x,y, cex=2, bg="black",pch=21)
#' #'     
#' #'     
#' #'   }
#' #' }
#' 
#' 
#' #' #' # THIS FUNCTION IS CURRENTLY DEPRICATED IN THIS VERSION. IT WILL BE UPDATED IN LATER VERSIONS
#' #' 
#' #' 
#' #' #' Plot multiple surfaces
#' #' #'
#' #' #' @param multi.surf A multi-surface pobject from multi.fnc.surface, or a list of adaptive surfaces 
#' #' #' @param ... optional parameters to pass onti plot_surf and par
#' #' #'
#' #' #' @return
#' #' #' @export
#' #' #'
#' #' #' @examples X
#' #' plot_multisurf <- function(multi.surf, main = NULL, ...){
#' #'   
#' #'   args <- list(multi.surf, main = NULL, ...)
#' #'   
#' #'   if(is.null(main)){
#' #'     main = names(multi.surf)
#' #'   }
#' #'   
#' #'   if(class(multi.surf) == "multi.Fnc.surf"){
#' #'     
#' #'     # lapply(multi.surf, FUN = function(X,...) plot_surf(surf = X$surface, ...), ...)
#' #'     for(i in 1:length(multi.surf)){
#' #'       
#' #'       plot_surf(multi.surf[[i]]$surf, main = main[i], ...)
#' #'       
#' #'     }
#' #'     
#' #'   }
#' #'   
#' #' }
#' 
#' # plot_adpt <- function(X, ...){
#' #     
#' # }
#' # 
#' # 
#' # plot_trans <- function(X, ...){
#' #     
#' # }
#' # 
#' # plot_pareto <- function(X, ...){
#' #     
#' #     
#' # }
#' 
#' 
#' 
