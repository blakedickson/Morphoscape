
# initialise --------------------------------------------------------------

rm(list= ls())

# install.packages("C:/Users/Blake/OneDrive/Documents/Morphoscape", repos = NULL,
#                  type = "source", force = T)

require(Morphoscape)


# load data ---------------------------------------------------------------
# 
# for (i in list.files("./data/turtles/", pattern = ".Rdata")){
#   
#   load(file = file.path("./data/turtles", 
#                         i) )
#   
# }
# 

load("./data/turtles.Rdata")

# plot fnc. data ----------------------------------------------------------

# load("./data/turtles/data.Rdata")

fnc.df <- fnc.dataframe(data)
fnc.surf <- multi.fnc.surface(data, npoints = 100)


# plot PCA and fnc traits -------------------------------------------------

pc.scores <- gPCA.MST$Scores
plot(pc.scores, pch = 21, bg = eco.col, cex = 2)

grp <- eco
grp.col <- eco.col

save(data, gpa, weights, chr.tree, grp, grp.col, file = "./data/turtles.Rdata")
