
## W calculates the height on the combined adaptive landscape at any point Zprime, given your functional landscapes (Fn) and a weight (w) Zprime = a
## corrdinate in morphospace (Pc1 and PC2 score) Fn = a list of functional surfaces wn = a vector of weights corressponding to each functional surface

Fnc.surf <- function(perf.surface, Zprime) {
  Zprime <- matrix(Zprime, ncol = 2)
  pred <- predict(perf.surface, x = Zprime[, 1], y = Zprime[, 2])
  return(pred)
}


W <- function(Zprime, wn, Fn) {

  wbar <- sum(sapply(Fn, FUN = Fnc.surf, Zprime = Zprime) * wn)

  return(wbar)
}


scale.z <- function(Z) {

  Z <- Z - min(Z)
  Z <- Z/max(Z)
  return(Z)
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


max_Wprime <- function(seed, Fn, wn, xmar, ymar, ...) {


  if (length(Fn[[1]]) == 3) {
    Fn.tmp <- list()
    for (i in 1:length(Fn)) {
      Fn.tmp[[i]] <- Fn[[i]]$poly

    }
    names(Fn.tmp) <- names(Fn)
    Fn <- Fn.tmp

  }


  Wprime <- optim(par = seed, fn = W, lower = c(xmar[1], ymar[1]), upper = c(xmar[2], ymar[2]), method = "L-BFGS-B", control = list(fnscale = -1),
                  wn = wn, Fn = Fn)

  return(Wprime)
}


lik_Zprime <- function(wn, Zprime, Fn, xmar = xmar, ymar = ymar, method = c("mean", "sum"), optimum = c("absolute", "relative", "dist")) {

  if (method == "sum") {
    Wprime <- mean(sapply(Zprime, FUN = W, Fn = Fn, wn = wn))

  } else {
    if (is.matrix(Zprime)) {
      Zprime <- colMeans(Zprime)
    }
    Wprime <- W(Zprime = Zprime, Fn = Fn, wn = wn)
  }

  max <- max_Wprime(seed = Zprime, Fn, wn, xmar = xmar, ymar = ymar)

  lik <- log((Wprime)/max$value)
  dis <- as.vector(1 - mean(dist(rbind(Zprime, max$par))))
  z <- (Wprime)

  X <- c(Wprime, lik, dis)
  names(X) <- c("z", "lik", "dis")
  return(X)
}

predict.surf <- function(surface, Zprime) {
  X <- expand.grid(surface$x, surface$y)
  colnames(X) <- c("x", "y")
  z <- as.vector(surface$z)
  XZ <- data.frame(X, z)
  ls <- surf.ls(np = 3, XZ)
  pred <- predict.trls(object = ls, x = as.vector(Zprime[, 1]), y = as.vector(Zprime[, 2]))
  return(data.frame(Zprime, pred))
}

euc.dists <- function(X1, X2) {
  dists <- matrix(NA, nrow = nrow(X1), ncol = nrow(X2))
  for (i in 1:nrow(X1)) {
    tmp <- vector()
    for (ii in 1:nrow(X2)) {

      tmp[ii] <- dist(rbind(X1[i, ], X2[ii, ]))
      # tmp[ii] <- sqrt( (X1[i,1] - X2[ii,1])^2 + (X1[i,2] - X2[ii,2])^2 )

    }
    dists[i, ] <- tmp

  }
  min.dists <- apply(dists, MARGIN = 1, min)
  mean.dist <- mean(min.dists)
  return(list(dists = dists, min.distance = min.dists, mean.distance = mean.dist))
}
