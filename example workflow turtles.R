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

species.df <- read.csv("./data/turtle humeri/sp.fnc.df.csv", row.names = 1)

grid.df <- read.csv("./data/turtle humeri/warp.fnc.df.csv", row.names = 1)

grid.df$hydro <- 1-grid.df$hydro
grid.df$fea <- 1-grid.df$fea



species.eco <- read.csv("./data/turtle humeri/eco.csv", row.names = 1)

coords <- species.df[,1:2]



# scale dataframes --------------------------------------------------------


grid_df <- fnc.dataframe(grid.df)


# calculate functional surfaces -------------------------------------------

kr_surf <- krige_surf(grid_df, new_data = coords ,hull = T, resample = 100)

plot_fn_kr(kr_surf)


# calculate all landscapes ------------------------------------------------




weights <- generate.weights(step = 0.05, nvar = 4)


#this function will automatically save to file at "./landscapeResults/allLandscapes.Rdata"
all_Lscape_data <- calc.all.lscps(weights, new_data = coords, fnc_data = grid_df)



# calculate best landscape for a given index of new_data ------------------



load("./landscapeResults/allLandscapes.Rdata")
all_Lscape_data$all_Wprime_surfs[[1]]$Wprime$new_data

GrpWprimeA <- calcGrpWprime(index = which(species.eco$Ecology=="S"), X = all_Lscape_data, verbose = F)
GrpWprimeB <- calcGrpWprime(index = which(species.eco$Ecology=="T"), X = all_Lscape_data, verbose = F)

GrpWprimeA$Wprime


# compare groups ----------------------------------------------------------

lands.grp.test(GrpWprimeA,GrpWprimeB)


groups <- list(A =GrpWprimeA, B = GrpWprimeB)
multi.lands.grp.test(groups)



# Plot landscapes ---------------------------------------------------------

#still need to make a universal plot function for any adaptive landscape surfaces






