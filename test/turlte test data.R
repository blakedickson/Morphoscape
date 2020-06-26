
# initialise --------------------------------------------------------------

rm(list= ls())

# install.packages("C:/Users/Blake/OneDrive/Documents/Morphoscape", repos = NULL,
#                  type = "source", force = T)

require(Morphoscape)


# load data ---------------------------------------------------------------
# 


load("./data/turtles.Rdata")


# plot PCA and fnc traits -------------------------------------------------

pc.scores <- pca$Scores
plot(pc.scores, pch = 21, bg = grp.col, cex = 2)

# plot fnc. data ----------------------------------------------------------


fnc.df <- fnc.dataframe(data)

multi.surf <- multi.fnc.surface(data, npoints = 100)


multi.surf$dors.sqrt$surface

class(multi.surf)
class(multi.surf$dors.sqrt)
class(multi.surf$dors.sqrt$surface)

surf <- multi.surf$dors.sqrt


par(mfrow = c(1,1))
plot_surf(surf = multi.surf$dors.sqrt, main = "dors.sqrt",
          points = pc.scores, pt.col = grp.col, axes = F, cex = 2)

length(multi.surf)

par(mfrow = c(3,2))
plot_multisurf(multi.surf, points = pc.scores,pt.col = grp.col, cex = 2)


