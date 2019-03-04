# Practical 5: multiple explanitory variables

library(lattice)
library(gplots)

seMean <- function(x) {
	x  <- na.omit(x) # get rid of missing values
	se <- sqrt(var(x)/length(x)) # calculate the standard error
	return(se)
}

setwd('H:/Biology/Statistics/Practical 5')
load('mammals.Rdata')
ls(mammals)
str(mammals)
mammals <- subset(mammals, select = c(GroundDwelling, TrophicLevel, logCvalue))
mammals <- na.omit(mammals)
str(mammals)

par(mfrow=c(1,2))
plot(logCvalue ~ TrophicLevel, data = mammals, 
	 subset  = GroundDwelling == 'No', main = "Not ground dwelling")
plot(logCvalue ~ TrophicLevel, data = mammals, 
	 subset  = GroundDwelling == 'Yes', main = "Ground dwelling")

# fucntion from lattice package
bwplot(logCvalue ~ TrophicLevel | GroundDwelling, data = mammals) 
# with trophic level as the subset instead 
bwplot(logCvalue ~ GroundDwelling | TrophicLevel, data = mammals)  

groups <- list(mammals$GroundDwelling, mammals$TrophicLevel)
groupMeans <- tapply(mammals$logCvalue, groups, FUN = mean)
print(groupMeans)
groupSE    <- tapply(mammals$logCvalue, groups, FUN = seMean)
print(groupSE)

# get upper and lower standard error height
upperSE <- groupMeans + groupSE
lowerSE <- groupMeans - groupSE
# create barplot
barMids <- barplot(groupMeans, ylim = c(0, max(upperSE)), beside = TRUE,
				   ylab = "log C value (pg)", col = c('red', 'royal blue'))
arrows(barMids, upperSE, barMids, lowerSE, ang = 90, code = 3, len = 0.1)

par(mfrow = c(1,2))
plotmeans(logCvalue ~ TrophicLevel, data = mammals, subset = GroundDwelling == 'Yes', 
		  connect = FALSE, ylim = c(0.8, 1.4))
text(0.6, 1.35, "a)")
plotmeans(logCvalue ~ TrophicLevel, data = mammals, subset = GroundDwelling == 'No', 
		  connect = FALSE, ylim = c(0.8, 1.4))
text(0.6, 1.35, "b)")

model <- lm(logCvalue ~ TrophicLevel + GroundDwelling, data = mammals)
par(mfrow = c(2, 2))
plot(model)
anova(model)
summary(model)

# data frame of combinations of variables
gd <- rep(levels(mammals$GroundDwelling), times = 3)
print(gd)
tl <- rep(levels(mammals$TrophicLevel), each = 2)
print(tl)
predVals <- data.frame(GroundDwelling = gd, TrophicLevel = tl)
predVals$predict <- predict(model, newdata = predVals)
print(predVals)

barMids <- barplot(groupMeans, ylim = c(0, max(upperSE)), beside = TRUE,
				   ylab = "log C value (pg)", col = c('red', 'royal blue'))
arrows(barMids, upperSE, barMids, lowerSE, ang = 90, code = 3, len = 0.1)
points(barMids, predVals$predict, col = 'green', pch = 5)