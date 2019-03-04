library(maptools)
setwd("C:/Users/jonchan/OneDrive/Documents/Uni work/Biodiversity and Conservation Biology/Practical 4")

sp <- readShapeSpatial("species.shp")
class(sp)
head(sp@data)

library(raster)
p1 <- raster('temperature.tif') # temperatute
p2 <- raster('precipitation.tif') # precipitation
p3 <- raster('elevation.tif') # elevation (DEM)
p4 <- raster('vegetation.tif')
# put all together as a single raster object:
p <- stack(p1, p2, p3, p4)
plot(p)
plot(p[[1]]) # first layer from p
plot(sp, add=T)

# extract predictor values at the species location:
preds <- extract(p, sp)
head(preds)
# Pred just has predictor variables. The response variable is in sp@data, column of Occurrence
# Let's include all together in a new data.frame:
df <- data.frame(preds, Occurrence=sp@data$Occurrence)
head(df)

# Fit a logistic regression 
m1 <- glm(Occurrence~., data=df, family=binomial)
summary(m1)
pr <- predict(p, m1, type='response')
plot(pr)

library(sdm)
# we already read species data (sp) and predictors (p)
d <- sdmData(sp, p)
m2 <- sdm(Occurrence~., data=d, methods = c('glm'))
pr2 <- predict(m2,p)
plot(pr2)
m3 <- sdm(Occurrence~., data=d, methods = c('glm','rf','svm'), test.perc=30)
roc(m3)