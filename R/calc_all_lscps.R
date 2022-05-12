calc_all_lscps <- function(kr_data, grid_weights, file = NULL, verbose = TRUE){

  if (!inherits(kr_data, "kriged_surfaces")){
    stop("'kr_data' must be a kriged_surfaces object, the output of a call to krige_surf().", call. = FALSE)
  }
  
  if (!inherits(grid_weights, "grid_weights")){
    stop("'grid_weights' must be a grid_weights object, the output of a call to generate_weights().", call. = FALSE)
  }
  
  func.names <- names(kr_data$autoKrige)
  
  all_Wprime_surfs <- vector("list", nrow(grid_weights))
  
  if (verbose) {
    cat("Calculating landscapes...\n")
    pb <- txtProgressBar(min = 0, max = nrow(grid_weights), style = 3)
  }
  
  for (i in 1:nrow(grid_weights)) {
    all_Wprime_surfs[[i]] <- calc.W.kr(grid_weights[i,],
                                       fnc_data = kr_data$dataframes,
                                       func.names = func.names)
    if (verbose) {
      setTxtProgressBar(pb, i)
    }
  }
  
  if (verbose) close(pb)
  
  all_lscps <- list(dataframes = kr_data$dataframes, 
                    all_Wprime_surfs = all_Wprime_surfs)
  
  class(all_lscps) <- "all_lscps"
  
  if (length(file) > 0) {
    if (length(file) != 1 || !is.character(file)) {
      warning("'file' must be a string containing the file path for the file to be saved. No file will be saved.", call. = FALSE)
    }
    else if (endsWith(tolower(file), ".rds")) {
      saveRDS(all_lscps, file = file)
    }
    else if (endsWith(tolower(file), "rdata")) {
      save(all_lscps, file = file)
    }
    else {
      warning("'file' must have an .rds or .rdata file extension. No file will be saved.", call. = FALSE)
    }
  }
  
  return(all_lscps)
}

calc_lscp <- function(kr_data, weights) {
  if (!inherits(kr_data, "kriged_surfaces")){
    stop("'kr_data' must be a kriged_surfaces object, the output of a call to krige_surf().", call. = FALSE)
  }
  
  fn_dataframe <- kr_data$dataframes
  func.names <- names(kr_data$autoKrige)
  
  if (!is.numeric(weights) || length(weights) != length(func.names)) {
    stop("'weights' must be a vector of weights, one for each functional characteristic in 'kr_data'.", call. = FALSE)
  }
  names(weights) <- func.names
  
  out <- calc.W.kr(weights,
                   fnc_data = fn_dataframe,
                   func.names = func.names)
  
  #Output is class "wtd_lscp"
  out
}

#Calculate Zprime values for a given vector of W
#fnc_data is list containing grid and (optionally) new_data
calc.W.kr <- function(W, fnc_data, func.names) {
  
  Wprime <- lapply(fnc_data, function(data) {
    data[func.names] <- sweep(data[func.names], 2, W, FUN = "*")
    Z <- rowSums(data[func.names])
    
    cbind(data, Z = Z)
  })
  
  out <- list(W = W, Wprime = Wprime)
  class(out) <- "wtd_lscp"
  
  out
}

print.wtd_lscp <- function(x, ...) {
  cat("A wtd_lscp object\n")
  cat("- weights:\n")
  print(round(x[["W"]], 4))
  if (!is.null(x[["Wprime"]][["new_data"]])) {
    cat("- new data:\n\t", nrow(x[["Wprime"]][["new_data"]]), " rows\n\t",
    "average Z = ", round(mean(x[["Wprime"]][["new_data"]][["Z"]]), 3), "\n", sep = "")
  }
}

print.all_lscps <- function(x, ...) {
  cat("An all_lscps object\n")
  cat("- functional characteristics:\n\t", paste(names(x[["dataframes"]][["grid"]])[-(1:2)], collapse = ", "), "\n", sep = "")
  cat("- number of landscapes:\n\t", length(x[["all_Wprime_surfs"]]), "\n", sep = "")
  cat("- weights incremented by:\n\t",
      round(x[["all_Wprime_surfs"]][[1]][["W"]][1] - x[["all_Wprime_surfs"]][[2]][["W"]][1], 4),
      "\n", sep = "")
  if (!is.null(x[["dataframes"]][["new_data"]])) {
    cat("- new data:\n\t", nrow(x[["dataframes"]][["new_data"]]), " rows\n", sep = "")
  }
  
  invisible(x)
}
