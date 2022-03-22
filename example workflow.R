rm(list = ls(all.names = TRUE))
gc()



require(Morphoscape)

require("alphahull")
require("hull2spatial")
require("sp")
require("automap")
require(viridis)

source("./R/landscape_functions.R")
source("./R/stats.R")
source("./R/plot_landscapes.R")

# load and coerce data ----------------------------------------------------



points <- read.csv("./data/jaws/points_pred.csv")[,-1]
coords <- as.matrix(points[,2:3])
colnames(coords) <- c("x", "y")

load("./data/jaws/exp_df.Rdata")


eco <- points$Factor




# calculate functional surfaces -------------------------------------------

kr_surf <- krige_surf(exp_df, new_data = coords, hull = T, resample = 100)

plot_fn_kr(kr_surf)


# calculate all landscapes ------------------------------------------------




weights <- generate.weights(step = 0.05, nvar = 4)


#this function will automatically save to file at "./landscapeResults/allLandscapes.Rdata"
all_Lscape_data <- calc.all.lscps(weights, new_data = coords, fnc_data = exp_df)



# calculate best landscape for a given index of new_data ------------------



load("./landscapeResults/allLandscapes.Rdata")
all_Lscape_data$all_Wprime_surfs[[1]]$Wprime$new_data

GrpWprimeA <- calcGrpWprime(index = c(1,2,3), X = all_Lscape_data, verbose = F)
GrpWprimeB <- calcGrpWprime(index = c(61,62,63), X = all_Lscape_data, verbose = F)

GrpWprimeA$Wprime


# compare groups ----------------------------------------------------------

lands.grp.test(GrpWprimeA,GrpWprimeB)


groups <- list(A =GrpWprimeA, B = GrpWprimeB)
multi.lands.grp.test(groups)



# Plot landscapes ---------------------------------------------------------

#still need to make a universal plot function for any adaptive landscape surfaces






