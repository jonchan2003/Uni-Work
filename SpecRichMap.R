setwd("D:/Workshop")

library(maptools)
s <- readShapeSpatial('REPTILES.SHP')
unique(s@data$family_nam)
s <- subset(s, family_nam == 'PHRYNOSOMATIDAE')
s@data$binomial <- factor(s@data$binomial)
unique(s@data$binomial) # name of species
plot(s)

library(raster)
r <- raster('base_map.grd')
par(mar=c(1,1,1,1))
plot(r, xlim = c(-1000, 1000))
plot(s, add=T)

u <- as.character(unique(s@data$binomial))
s.temp <- subset(s,binomial == u[1])
# just create a temporary column with values of 1, which later will be used to rasterize the species
s.temp@data$f <- 1
plot(s.temp)
plot(r) # base map
plot(s.temp,add=T)

rr <- raster(r)
for (uu in u) {
  s.temp <- subset(s,binomial == uu)
  s.temp@data$f <- 1
  r.temp <- rasterize(s.temp, r, field='f')
  rr <- addLayer(rr,r.temp)
}

rich <- calc(rr,sum,na.rm=T)
plot(rich)

rich <- mask(rich,r)
plot(rich, xlim = c(-170, -30))
