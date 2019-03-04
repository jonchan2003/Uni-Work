library(raster)
library(maptools)
library(vegan)

setwd("C:/Users/jonchan/OneDrive/Documents/Uni work/Biodiversity and Conservation Biology/Coursework 1")

Species <- read.csv("species_matrix.csv", row.names = 1)
Species <- as.matrix(Species)
Species_XY <- read.csv("sites_xy.csv", row.names = 1)
basemap <- readShapePoly("poly_basemap.shp")
plot(basemap, col = "light grey", border = "dark grey")
points(Species_XY, pch = 10, col = "red")

Species_SR <- rowSums(Species)
source("plotting_Colours.R")
Colour <- plotcols(Species_SR)

plot(basemap, col = "light grey", border = "dark grey")
points(Species_XY, pch = 21, bg = Colour, cex = 2)

Chaet_Sor <- betadiver(Species, method = "hk")
Chaet_Sim <- betadiver(Species, "sim")

Chaet_Sor_NMDS <- metaMDS(Chaet_Sor, k = 2)
plot(Chaet_Sor_NMDS)
Chaet_Sim_NMDS <- metaMDS(Chaet_Sim, k = 2)
plot(Chaet_Sim_NMDS)

Chaet_Sor_NMDS_cols <- plotcols2D(Chaet_Sor_NMDS$points)
plot(Chaet_Sor_NMDS$points, pch = 21, bg = Chaet_Sor_NMDS_cols, cex = 1.5, main = " Plethodontidae Sørensen")
plot(basemap, col = "light grey", border = "dark grey")
title("Plethodontidae Sørensen")
points(Species_XY, pch = 21, bg = Chaet_Sor_NMDS_cols, cex = 2)

Chaet_Sim_UPGMA <- hclust(Chaet_Sim, method = "average")
Chaet_Sim_Bioregions <- cutree(Chaet_Sim_UPGMA, k = 6)
Chaet_Sim_Bioregions_cols <- plotcols2D_clust(Chaet_Sim_NMDS$points, Chaet_Sim_Bioregions)
plot(basemap, col = "light grey", border = "dark grey")
title("Plethodontidae BetaSim Regions")
points(Species_XY, pch = 21, bg = Chaet_Sim_Bioregions_cols, cex = 1.5)

Chaet_Sor_UPGMA <- hclust(Chaet_Sor, method = "average")
Chaet_Sor_Bioregions <- cutree(Chaet_Sor_UPGMA, k = 6)  
Chaet_Sor_Bioregions_cols <- plotcols2D_clust(Chaet_Sor_NMDS$points, Chaet_Sor_Bioregions)
plot(basemap, col = "light grey", border = "dark grey")
title("Plethodontidae BetaSor Regions")
points(Species_XY, pch = 21, bg = Chaet_Sor_Bioregions_cols, cex = 2)