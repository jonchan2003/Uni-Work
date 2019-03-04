setwd("downloads/BioRegPrac")

Chaet_dat <- read.csv("Chaetodontidae.csv", row.names = 1)
Chaet_dat <- as.matrix(Chaet_dat)
str(Chaet_dat)
Chaet_SR <- rowSums(Chaet_dat)
# gives the total number of presences per row (i.e. survey location)
summary(Chaet_SR)  # some summary stats
hist(Chaet_SR)  # a histogram

Fish_XY <- read.csv("Fish_XY.csv", row.names = 1)
library(maptools)
#now we are ready to load and plot the basemap shapefile
basemap <- readShapePoly("IndoPac.shp")
plot(basemap, xlim = c(-17000000, 7000000), ylim = c(-360000, 290000), col = "light grey", border = "dark grey")
title("Butterflyfish Survey Locations")
# notice that, when plotting the map, the code above manually sets the X and Y limits, 
# as well as the colours of the shapes and borders. Feel free to change the colours. 
# You will find a pdf with a list of R colours in the folder for this practical. 
# The X and Y limits are set to match the fish survey locations, so best to leave them.
# now add the fish survey locations
points(Fish_XY, pch = 10, col = "red") #pch sets the plotting character
text(Fish_XY - (10^5.2), rownames(Fish_XY), cex =0.4)
# plots the names of the locations (which are the rownames). Coordinates are offset to avoid overlap. 
# cex sets the font size (you may need to alter this).

source("plotting_Colours.R")
ChaetSR_cols <- plotcols(Chaet_SR)

ChaetSR_cols <- ChaetSR_cols[rownames(Fish_XY)]
plot(basemap, xlim = c(-17000000, 7000000), ylim = c(-360000, 290000), col = "light grey", border = "dark grey")
title("Butterflyfish Species Richness")
points(Fish_XY, pch = 21, bg = ChaetSR_cols, cex = 2)

Selected_Sites <- c("Jarvis__Island", "Kingman_Reef", "Tuamotu")
plot(basemap, xlim = c(-17000000, 7000000), ylim = c(-360000, 290000), col = "light grey", border = "dark grey")
title("Butterflyfish Survey Locations")
points(Fish_XY, pch = 21, bg = ChaetSR_cols, cex = 2)
#the previous code plot everything as before, the following two lines select only the sites we are currently interested in
points(Fish_XY[Selected_Sites,], pch = 10, col = "green", cex =3) #pch sets the plotting character
text(Fish_XY[Selected_Sites,] - (10^5.2), Selected_Sites, cex =0.4)

betasor <- function(a,b,c){
  result <- ((b+c)/((2*a)+b+c))
  return(result)
}
betasor(7, 0, 11)
betasor(6, 1, 22)
betasor(15, 3, 13)

betasim <- function(a,b,c){
  result <- (min(b,c)/(a+min(b,c)))
  return(result)
}
betasim(7, 0, 11)
betasim(6, 1, 22)
betasim(15, 3, 13)

library(vegan)
Chaet_Sor <- betadiver(Chaet_dat, method = "hk")
Chaet_Sim <- betadiver(Chaet_dat, "sim")
# calculates "dist" objects, which are the lower triangles of the pairwise matrices. Have a look at how they are structured:
str(Chaet_Sor)
#if you wanted to have a full matrix instead you can convert dist objects to a matrix using as matrix. For example the following code gives the results for the sites we looked at manually.
as.matrix(Chaet_Sor)[ Selected_Sites, Selected_Sites]
as.matrix(Chaet_Sim)[ Selected_Sites, Selected_Sites]

Chaet_Sor_NMDS <- metaMDS(Chaet_Sor, k = 2)
plot(Chaet_Sor_NMDS, main = "Butterflyfish Sørensen NMDS")
Chaet_Sim_NMDS <- metaMDS(Chaet_Sim, k = 2)
plot(Chaet_Sim_NMDS, main = "Butterflyfish BetaSim NMDS")

Chaet_Sor_NMDS1_cols <- plotcols(Chaet_Sor_NMDS$points[,1])
plot(Chaet_Sor_NMDS$points, pch = 21, bg = Chaet_Sor_NMDS1_cols, cex = 1.5, main = "Butterflyfish Sørensen NMDS", sub = "coloured according to NMDS1")
plot(basemap, xlim = c(-17000000, 7000000), ylim = c(-360000, 290000), col = "light grey", border = "dark grey")
points(Fish_XY, pch = 21, bg = Chaet_Sor_NMDS1_cols, cex = 2)

Chaet_Sor_NMDS_cols <- plotcols2D(Chaet_Sor_NMDS$points)
plot(Chaet_Sor_NMDS$points, pch = 21, bg = Chaet_Sor_NMDS_cols, cex = 1.5, main = "Butterflyfish Sørensen")
plot(basemap, xlim = c(-17000000, 7000000), ylim = c(-360000, 290000), col = "light grey", border = "dark grey")
title("Butterflyfish Sørensen")
points(Fish_XY, pch = 21, bg = Chaet_Sor_NMDS_cols, cex = 2)

Chaet_Sim_NMDS_cols <- plotcols2D(Chaet_Sim_NMDS$points)
plot(Chaet_Sim_NMDS$points, pch = 21, bg = Chaet_Sim_NMDS_cols, cex = 1.5, main = "Butterflyfish BetaSim ")
plot(basemap, xlim = c(-17000000, 7000000), ylim = c(-360000, 290000), col = "light grey", border = "dark grey")
title("Butterflyfish BetaSim")
points(Fish_XY, pch = 21, bg = Chaet_Sim_NMDS_cols, cex = 2)

Chaet_Sim_UPGMA <- hclust(Chaet_Sim, method = "average")
Chaet_Sim_Bioregions <- cutree(Chaet_Sim_UPGMA, k = 3)
# here I have gone for three clusters, feel free to change this if you think there are more (or less)
#plot the new regions
Chaet_Sim_Bioregions_cols <- plotcols2D_clust(Chaet_Sim_NMDS$points ,Chaet_Sim_Bioregions)
plot(basemap, xlim = c(-17000000, 7000000), ylim = c(-360000, 290000), col = "light grey", border = "dark grey")
title("Butterflyfish BetaSim Regions")
points(Fish_XY, pch = 21, bg = Chaet_Sim_Bioregions_cols, cex = 2)

Chaet_Sor_UPGMA <- hclust(Chaet_Sor, method = "average")
Chaet_Sor_Bioregions <- cutree(Chaet_Sim_UPGMA, k = 3)
# here I have gone for three clusters, feel free to change this if you think there are more (or less)
#plot the new regions
Chaet_Sor_Bioregions_cols <- plotcols2D_clust(Chaet_Sor_NMDS$points ,Chaet_Sor_Bioregions)
plot(basemap, xlim = c(-17000000, 7000000), ylim = c(-360000, 290000), col = "light grey", border = "dark grey")
title("Butterflyfish BetaSor Regions")
points(Fish_XY, pch = 21, bg = Chaet_Sor_Bioregions_cols, cex = 2)

# Q1: Varies significantly by loation. With a range from 5 to 59, mean of 27.88. 
# Q2: Yes expect there to be some sort of geographic pattern. 
# Q3: Yes, species richness is highest in the south east asia region, southern Japan and Great barrier reef
# Q4: Temperature, habitat avaialability these area is high in coral reef cover. 
# Q5: Yes, differnt species present at differnt locations 
# Q6: Soutern Japan, Great Barrier reef, two sites within the cluster. Because provides diversoity and maximum SR cover
# Q7: Kinfsman Reef and Jarvis Island 
# Q8: Tuamotu, most number of species that differ from the otehr two sites 
# Q9: Jarvis Island hs the highest Sorenson index value, hence least number of shared species
# Q10: Kingsman reef
# Q11: betasor calculates the number of different species, whereas betasim loks at number of unique species
# Q12: Depends on what you are looking for, can use both 
# Q13: Two clustered sports. One large group to the right, small one of the left 
# Q14: Similar beta diversity scores because they are geographically close together and there is dispersal limitation, similar habitats 
# Q15: Appears to be dispersal limitation occuring. Difference between Indian ocean and Pacific ocean show biggest difference in colour scheme, opposite ends
# Q16: Both show a similar pattern, except that betaSim shows seperate cluster in guld strait & arabian sea region
# Q18: Patterns are fairly consistent, except that betaSim shows seperate cluster in guld strait & arabian sea region to be more distict from Pacfic cluster
# Q19: Shows distinct geographical regions, rather than a gradient. 
# Q20: Reflects the clustering the the ordination quite well. Reflects the clusters seen in ordination. Though there may be some small seperate clusters in Pacific clsuter
# Q21: Dispersal limitation, habitat preferences (salinity, temperature)
# Q22: phylogenetic data or trait data(e.g. temperature preference, )





