#Making datasets
warps <- read.csv("./data/turtle humeri/warp.fnc.df.csv", row.names = 1)

warps$hydro <- 1-warps$hydro
warps$fea <- 1-warps$fea

save(warps, file = "data/warps.rda")

species.df <- read.csv("./data/turtle humeri/sp.fnc.df.csv", row.names = 1)
species.eco <- read.csv("./data/turtle humeri/eco.csv", row.names = 1)

species <- cbind(species.df[1:2], species.eco)

save(species, file = "data/species.rda")