
# initialise --------------------------------------------------------------

rm(list= ls())

# install.packages("C:/Users/Blake/OneDrive/Documents/Morphoscape", repos = NULL,
#                  type = "source", force = T)

require(Morphoscape)


# load data ---------------------------------------------------------------
# 
for (i in list.files("./test/turtles/", pattern = ".RData")){

  load(file = file.path("./test/turtles",
                        i) )

}


# load("./data/turtles.Rdata")

# plot fnc. data ----------------------------------------------------------

# load("./data/turtles/data.Rdata")

fnc.df <- fnc.dataframe(data)
fnc.surf <- multi.fnc.surface(data, npoints = 100)


# plot PCA and fnc traits -------------------------------------------------

pc.scores <- gPCA.MST$Scores
plot(pc.scores, pch = 21, bg = eco.col, cex = 2)

grp <- eco
grp.col <- eco.col
pca <- gPCA.MST


weights.df.list <- list(opt.weights.M,opt.weights.S,opt.weights.T)

tools::checkRdaFiles("./test/turtles/")


save(data, pca, weights, weights.df.list, chr.tree, grp, grp.col, 
     file = "./data/turtles.RData")
