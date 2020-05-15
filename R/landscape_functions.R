
# this function cooerces many functional traits into a 3d array.
# Data are coercced into the following structure:
# dims: 1= observations (warps), 2 = x,y,z coordinates, 3= functional traits

#' Coerces XY coordinate data and covariate data into a list, or optionally into
#'    a 3D array
#'
#' @param x,y A numeric vectors with x,y coordinates of observations in
#'     morphospace
#' @param z A Numeric vector or matrix containing covariate observations corresponding to
#'     x,y coordinates. Can contain more than one covariate
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
fnc.dataframe <- function(x, y, z, row.names, func.names=NULL, array = F, scale = T){
    z <- as.matrix(z)

    if (scale){
        if (ncol(z)<1){
            z  <- apply(z, MARGIN = 2, FUN = function(z){
                z <- (z - min(z))
                z <- z / max(z)
            }
            )

        } else{
            z <- (z - min(z))
            z <- z / max(z)}
    }

    if(array){
        fnc.array <- array(dim=c(nrow(z), 3, ncol(z)),
                           dimnames = list(row.names,c("x","y","z"),func.names))

        for (i in 1:ncol(z)){
            fnc.array[,,i] <- cbind(x,y,as.numeric(z[,i]))
        }

    } else{
        fnc.array <- list()
        for (i in 1:ncol(z)){

            fnc.array[[i]] <- cbind(x,y,as.numeric(z[,i]))
            colnames(fnc.array[[i]]) <- c("x","y","z")
        }

        names(fnc.array) <- func.names
    }

    return(fnc.array)
}


#' Produces a 3D polynomial surface of a single functional dataframe.
#'     Useful for testing and checking data
#'
#' @param X An x,y,z funtional dataframe produced by fnc_dataframe
#' @param npoly Numeric. Determines the degree of polynomial to apply to the
#'     surface. Usually 3rd order polynomials produce the best fit
#' @param npoints Numeric. Optional argument to up or downsample the number x,y
#'     grid points. Defaults to the number of observations in X
#' @param fnc.name Optional string to label the functional surface
#'
#' @return Returns a list of polynomial surface objects:
#'     poly: the polynomial model applied to X
#'     summary: the polynomial model summary
#'     surface: the polynomial surface fit to X
#' @export
#'
#' @examples X
fnc.surface <- function(X,method = "poly", npoints = NULL, fnc.name = NULL, range = NULL, ...){
    X <- as.matrix(X)

    X <- na.omit(X)

    x <- X[,1]
    y <- X[,2]
    z <- X[,3]


    if (is.null(range)){
        range <- rbind(range(x),
                       range(y))

    }

    if (is.null(npoints)){
        npoints = length(x)
    }

    if (!is.null(fnc.name)){
        print(fnc.name)
    }

    if (method == "poly"){
        npoly = 3
        poly <- surf.ls(np = npoly, x = x, y = y, z = z) #fit polynomial least squares trend surface
        summary(poly)
        poly.surf<-trmat(poly, range[1,1], range[1,2],
                         range[2,1], range[2,2], npoints) # evaluate grid points over surface
        poly.surf$z <- scale.z(poly.surf$z)
        attr(poly.surf, "Class") <- "surf"
        fn.surf <- list(poly = poly, surface = poly.surf)
        # attr(fn.surf,"Class") <- "fn.surf"
        
        return(fn.surf)
    }

    ### ADD CLASS AND S3 METHOPDS FOR PLOTTING
    if (method == "kriging"){
        Kr <- kriging(x = x, y = y, response = z)
        
        return(Kr)

    }
    return(list(poly = poly, surface = poly.surf))
}


#' Produces a list of polynomial surface fits from multiple functional
#'     dataframes. Is essentailly an apply wrapper for fnc.surface
#'
#' @param X A list of x,y,z functonal dataframes from 'fnc_dataframe'
#' @param npoly Numeric. Determines the degree of polynomial to apply to the
#'     surface. Usually 3rd order polynomials produce the best fit
#' @param npoints Numeric. Optional argument to up or downsample the number x,y
#'     grid points. Defaults to the number of observations in X
#'
#' @return Returns a multi.fnc.surface object. A list containing N number of
#'     functional surfaces. See fnc.surface for details
#' @export
#'
#' @examples X
multi.fnc.surface <- function(X, method = "poly", npoints = NULL,...){

    multi.surf <- list()
    for(l in 1:length(X)){
        multi.surf[[l]] <- fnc.surface(X[[l]], method = method,
                                       fnc.name = names(X)[l],...)
    }
    names(multi.surf) = names(X)
    # attr(multi.surf,"Class") <- "multi.Fnc.surf"
    return(multi.surf)
}





#' Calculate combined surface calculate W on the adaptive landscape
#'  W = w1*F1 + w2*F2 (Polly et al, 2016)
#'
#' @param Fn A multi.surf object from multi.fnc.surface
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
adap.surf <- function(Fn, wn, xmar, ymar, n, ...) {

    if(length(Fn[[1]]) == 3) {
        Fn.tmp <- list()
        for (i in 1:length(Fn)){
            Fn.tmp[[i]] <-Fn[[i]]$poly

        }
        names(Fn.tmp) <- names(Fn)
        Fn <- Fn.tmp

    }


    dx <- (xmar[2] - xmar[1]) / n
    dy <- (ymar[2] - ymar[1]) / n
    x  <- seq(xmar[1], xmar[2], dx)
    y  <- seq(ymar[1], ymar[2], dy)
    Zprime.grid <- expand.grid(x, y)
    zraw <- matrix(apply(Zprime.grid, 1, FUN=W, wn, Fn),
                   nrow=length(x), ncol=length(y), byrow= F)

    z <- (zraw - min(zraw))
    z <- z / max(z)

    surface <- list(x = x, y = y, z = z)
    attr(surface, "Class") <- "surf"
    
    adap.surface <- list(surface = surface,
                         zraw = zraw,model=list(Fn=Fn,wn=wn))
    attr(adap.surface, "Class") <- "adap.lscp"

    return(adap.surface)
}



#' Generate a generic dataframe containing weight combinations. Can be used in
#'   search.w.exhaustive, or to split jobs for distributed computing
#'
#' @param step Numeric. Degree by which to vary weights by.
#' @param nvar Numeric. Number of variables (columns) to generate
#' @param varnames Optional. Names for variables
#' @param time.est Optional. Estimate length of time to perform
#'   search.w.exhaustive
#' @param verbose Logical. Return dataframe. Used for silencing in time.est
#' @param ...
#'
#' @return
#' @export
#'
#' @examples X
generate.weights <- function(step, nvar, varnames = NULL, time.est = F,
                             verbose = T, ...) {
    n = 1/step
    weights<- parti(n, nvar)/n

    if (!is.null(varnames)){
        colnames(weights) <- varnames
    }  else colnames(weights) <- 1:nvar

    print(paste(nrow(weights),"rows generated"))

    if (time.est) {
        xmar<-range(Fn[[1]][[1]]$x)
        ymar<-range(Fn[[1]][[1]]$y)
        time.est(num.perm = nrow(weights), nvar = nvar, Fn = Fn,
                 xmar = xmar, ymar = ymar)
    }

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
time.est <- function(num.perm, nvar, Fn, xmar, ymar) {

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


# method can be either 'sum' or 'mean'. Sum will caculate w for all points given and sum them, mean with calculate only the mean of the points given.
# default is 'mean' which is faster optimum can be either 'absolute' or 'relative'.  absolute calculates the absolute height on the landscape, where
# relative compares it to the wmax, the heightest point on the landscape.  'Relative' can sometimes give odd results and should be used with caution

#' Calculate the optimum weights for each trait given a position, or group in
#' morphospace
#'
#' @param Zprime Numeric vector of length=2, or Nx2 matrix containing x,y
#'     coordiates to optimize for
#' @param Fn A multi.fnc.surface list containing traits to model for
#' @param weights.df An optional matrix containing all possible combinations of
#'      wn given n number of traits.
#' @param step Optional numeric. The degree by which to vary wn by. Required if
#'     weights.df is not provided. Smaller step sizes require exponentially more
#'      computation time.
#' @param xmar,ymar Numeric vector containing min/max margins for morphospace
#' @param Cluster logical. Split computation across multiple machines.
#'     Currently not working
#' @param method Method by which to summarize optimization results when multiple
#'      Zprime coordintes are provided. Both produce similar results, but have
#'       not been stress tested.
#' @param optimum Criteria to optimize for and sort results by. Currently only
#'     "absolute" height on the landscape is supported, though all optimization
#'     metrics are returned. See details.
#'
#' @return A matrix containing three optimization metrics for all combinations
#'     of weights as provided by weights.df, or generated by 'step'.
#'     Automatically sorted by 'absolute'.
#' @export
#' @details
#'
#' @examples X
search.w.exhaustive <- function(step =NULL, Zprime, Fn, Cluster = F,
                                method = c("mean","sum"),
                                optimum = c("absolute"),
                                xmar=xmar, ymar=ymar,weights.df = NULL){

    if (is.vector(Zprime)){
        Zprime <- Zprime[1:2]
    }
    if (length(dim(Zprime))==2){
        Zprime <- Zprime[,1:2]
    }


    if(length(Fn[[1]]) == 3) {
        Fn.tmp <- list()
        for (i in 1:length(Fn)){
            Fn.tmp[[i]] <-Fn[[i]]$poly

        }
        names(Fn.tmp) <- names(Fn)
        Fn <- Fn.tmp

    }

    if(is.null(weights.df)){
        if(is.null(weights.df)){
            stop("no step size provided. Please provide either a step size or weights dataframe")
        }
        weights.df <- generate.weights(step,nvar=length(Fn))
        colnames(weights.df) <- names(Fn)

    }
    # Cluster mode is not currently working | must troubleshoot
    if (Cluster){
        no_cores <- detectCores()
        cl <- makeCluster(no_cores)
        clusterExport(cl, varlist=c("max_Wprime","DEoptim",
                                    "Zprime","Fnc.surf","W","lik_Zprime",
                                    "Fn","xmar","ymar"),
                      envir=environment() )

        lik <- parApply(cl = cl, X = weights.df, MARGIN = 1, FUN=lik_Zprime,
                        Zprime=Zprime, Fn = Fn,
                        xmar=xmar, ymar=ymar)
        stopCluster(cl)

    } else{

        lik <- apply(X = weights.df, MARGIN = 1, FUN=lik_Zprime,
                     Zprime=Zprime, Fn = Fn,
                     xmar=xmar, ymar=ymar,
                     method = method,
                     optimum = optimum)

    }

    colnames(weights.df)[1:length(Fn)] <- names(Fn)
    weights.df <- cbind(weights.df, t(lik))

    print("Done!")
    return(weights.df)
}


#' Calculate pareto front between two landscapes
#'
#' @param surf.1,surf.2 surface objects
#' @param plot logical, plot pareto front
#' @param a sequence from 1-0 to calculate the pareto transtion
#' @param ... additional parameters for plot.surf
#' @param smooth optional c("spline", "kernel"), determines which smoothing
#'   method to use. The "spline" method will fail if mulitple values of Y exist
#'   for a given X, in which case use "kernel"
#'
#' @return a list containing the pareto transition
#' @export
#'
#' @examples X
pareto <- function(surf.1,surf.2,plot=F,a=NULL, smooth = c("spline","kernel"), ...){

    if (is.null(a)){
        a <- seq(1,0, by= -0.025)
    }
    b <- 1-a
    comb <- surf.1

    z <- vector()
    x <- vector()
    y <- vector()

    i=1
    for(i in 1:length(a)){
        comb$z <- rep(NA,length(surf.1$z))
        comb$z <- surf.1$z*a[i] + surf.2$z*b[i]
        if (plot){
          dev.off()
          plot_surf(comb,main="Combined",...)

        }


        z[i] <- max(comb$z)
        x[i] <- comb$x[which(comb$z==max(comb$z),arr.ind = T)[1]]
        y[i] <- comb$y[which(comb$z==max(comb$z),arr.ind = T)[2]]

    }


    surf.1$max$x <- surf.1$x[which(surf.1$z==max(surf.1$z),arr.ind = T)[1]]
    surf.1$max$y <- surf.1$y[which(surf.1$z==max(surf.1$z),arr.ind = T)[2]]
    surf.1$max$z <- max(surf.1$z)
    surf.1.max <- data.frame(surf.1$max)

    surf.2$max$x <- surf.2$x[which(surf.2$z==max(surf.2$z),arr.ind = T)[1]]
    surf.2$max$y <- surf.2$y[which(surf.2$z==max(surf.2$z),arr.ind = T)[2]]
    surf.2$max$z <- max(surf.2$z)
    surf.2.max <- data.frame(surf.2$max)
    pareto<-data.frame(x,y,z)
    
    if (smooth == "spline"){
        pareto.line <- smooth.spline(pareto[,1:2], spar = 0.6)
    }
    if (smooth == "Kernel"){
        pareto.line <- smoothr::smooth_ksmooth(as.matrix(pareto[,1:2]),
                                               smoothness=10)
    }
    
    
    
    return(list(pareto=pareto, pareto.line = pareto.line, 
                surf.1.max=surf.1.max,surf.2.max=surf.2.max))
}



#' Sum any number of adaptive landscapes
#'
#' @param landscapes a list of adaptive lanscapes
#'
#' @return Returns a combined adpative landscape
#' @export
#'
#' @examples X
sum.surface <- function(landscapes) {
    L <- list()
    for (l in 1:length(landscapes)) {
        L[[l]] <- landscapes[[l]]$surface$z
    }
    lscp.sum <- Reduce("+", L)
    L <- list(x = landscapes[[1]]$surface$x, y = landscapes[[1]]$surface$y, z = lscp.sum)
    class(L) <- "surf"
    return(L)
}

#' Calculate a transition landscape between two landscapes
#'
#' @param X,Y A $surface onject
#' @param binary Logical. Calcuate as a binary transition
#'   (STILL IN DEVELOPMENT). Defaults to FALSE
#'
#' @return Returns a $surface object
#' @export
#'
#' @examples X
trans.surface <- function(X, Y, binary = F) {
    L <- X
    L$surface$z <- (X$surface$z + 1)/(Y$surface$z + 1)

    if (binary) {
        bn <- L$surface$z > 1
        L$surface$z[bn] <- 1
    }
    class(L) <- "surf"
    return(L)
}





#' Calculate the best model fits
#'
#' @param X A weights matrix generated from search.w.exhaustive()
#' @param sortby c("z", "lik", "dis"). Which optimization vector to sort by.
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
calc.best.wn <- function(X, sortby = "lik", percentile = 0.99, method=c("quantile")){
    X <- X[ order( X[ , sortby], decreasing = T), ]
    if(method=="quantile"){
        X.top <- X[ X[ , sortby] > quantile( X[, sortby], probs = percentile) , ]
    }
    if(method=="chi-squared"){
        critval <- qchisq(1 - percentile, 1)
        x <- ( -2*( X$lik - X$lik[1]) )
        X.top <- X[ 1:length( which( x < critval) ), ]
    }
    wn <- colMeans( X.top[, -match(c("z", "lik", "dis"), colnames(X))] )
    wn.se <- apply(X.top[,-match(c("z", "lik", "dis"), colnames(X))],2, plotrix::std.error)
    wn.sd <- apply(X.top[,-match(c("z", "lik", "dis"), colnames(X))],2, sd)
    wn.range <- apply(X.top[,-match(c("z", "lik", "dis"), colnames(X))],2, range)
    return(list(wn=wn, wn.se=wn.se, wn.sd=wn.sd, wn.range=wn.range))
}



#' Pairwise significance testing between group model fits
#'
#' @param X1,X2 Dataframe containing Wn model fits for two groups  
#' @param p.val 
#'
#' @return List containing p.values and matching weights
#' @author Katrina Jones
#' @export
#'
#' @examples X
lscp.pairwise<-function(X1, X2, p.val = 0.99){
    #get top 1% based on likelihood
    besta<-grpa[order(grpa$lik,decreasing = T),][1:round(nrow(grpa)*0.01),]
    bestb<-grpb[order(grpb$lik,decreasing = T),][1:round(nrow(grpb)*0.01),]
    #Check for matching models
    m<-match(besta$X,bestb$X)
    n.match<-length(m[!is.na(m)])
    p.match<- n.match/length(m)
    return(list(n.match= n.match, p.val=p.match, matching=besta[na.omit(m),]))
}


