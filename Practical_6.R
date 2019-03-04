# Practical 6: more variables and interactions

setwd('H:/Biology/Statistics/Practical 6')
load('mammals.Rdata')

model <- lm(logCvalue ~ TrophicLevel * GroundDwelling, data = mammals)
par(mfrow = c(2,2), mar = c(3,3,1,1), mgp = c(2, 0.8,0))
plot(model)

anova(model)
summary(model)

# generates a data frame of combinations of variables
gd <- rep(levels(mammals$GroundDwelling), times = 3)
print(gd)
tl <- rep(levels(mammals$TrophicLevel), each = 2)
print(tl)
predVals <- data.frame(GroundDwelling = gd, TrophicLevel = tl)
# predict using the new data frame
predVals$predict <- predict(model, newdata = predVals)
print(predVals)

odonata <- read.csv('GenomeSize.csv')
odonata$logGS <- log(odonata$GenomeSize)
odonata$logBW <- log(odonata$BodyWeight)

odonModel <- lm(logBW ~ logGS * Suborder, data = odonata)
anova(odonModel)
summary(odonModel)

# get the range of the data
rng <- range(odonata$logGS)
# get a sequence from the min to the max with 100 equally spaced values
span <- seq(rng[1], rng[2], length = 100)
# get a data frame of new data for the order
ZygoVals <- data.frame(logGS = span, Suborder = "Zygoptera")
ZygoPred <- predict(odonModel, newdata = ZygoVals, se.fit = TRUE) # get the predictions and standard error
# repeat for anisoptera
AnisoVals <- data.frame(logGS = span, Suborder = "Anisoptera")
AnisoPred <- predict(odonModel, newdata = AnisoVals, se.fit = TRUE)
# plot the scatterplot of the data
plot(logBW ~ logGS, data = odonata, col = Suborder)
# add the predicted lines
lines(AnisoPred$fit ~ span, col = "black")
lines(AnisoPred$fit + AnisoPred$se.fit ~ span, col = "black", lty = 2)
lines(AnisoPred$fit - AnisoPred$se.fit ~ span, col = "black", lty = 2)

lines(ZygoPred$fit ~ span, col = "red")
lines(ZygoPred$fit + ZygoPred$se.fit ~ span, col = "red", lty = 2)
lines(ZygoPred$fit - AnisoPred$se.fit ~ span, col = "red", lty = 2)