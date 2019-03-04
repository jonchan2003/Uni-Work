# Practical 4 of statistics. Looking at ANOVA, analysis of variance
library(gplots)
# get standard error of the mean from a set of values (x)
seMean <- function(x) {
	x  <- na.omit(x) # get rid of missing values
	se <- sqrt(var(x)/length(x)) # calculate the standard error
	return(se)
}

setwd("H:/Biology/Statistics/Practical 4")
mammals <- read.csv('MammalData.csv')
str(mammals)
summary(mammals)

plot(meanCvalue ~ TrophicLevel, data = mammals)
mammals$logCvalue <- log(mammals$meanCvalue)
plot(logCvalue ~ TrophicLevel, data = mammals)
plot(logCvalue ~ GroundDwelling, data = mammals)

trophMeans <- tapply(mammals$logCvalue, mammals$TrophicLevel, FUN = mean, na.rm = TRUE)
print(trophMeans)
trophSE    <- tapply(mammals$logCvalue, mammals$TrophicLevel, FUN = seMean)
print(trophSE)
upperSE <- trophMeans + trophSE  # to get the upper and lower limts of the error bars
lowerSE <- trophMeans - trophSE
barMids <- barplot(trophMeans, ylim = c(0, max(upperSE)), ylab = 'log C value (pg)')
arrows(barMids, upperSE, barMids, lowerSE, ang = 90, code = 3)

dwellingMeans <- tapply(mammals$logCvalue, mammals$GroundDwelling, FUN = mean, na.rm = TRUE)
print(trophMeans)
dwellingSE    <- tapply(mammals$logCvalue, mammals$GroundDwelling, FUN = seMean)
print(trophSE)
upperSE <- dwellingMeans + dwellingSE  # to get the upper and lower limts of the error bars
lowerSE <- dwellingMeans - dwellingSE
barMids <- barplot(dwellingMeans, ylim = c(0, max(upperSE)), ylab = 'log C value (pg)')
arrows(barMids, upperSE, barMids, lowerSE, ang = 90, code = 3)

par(mfrow = c(1, 2))
plotmeans(logCvalue ~ TrophicLevel, data = mammals, p = 0.95, connect = FALSE)
plotmeans(logCvalue ~ GroundDwelling, data = mammals, p= 0.95, connect = FALSE)

trophicLM <- lm(logCvalue ~ TrophicLevel, data = mammals)
summary(trophicLM)
anova(trophicLM)

dwellingLM <- lm(logCvalue ~ GroundDwelling, data = mammals)
summary(dwellingLM)
anova(dwellingLM)

par(mfrow = c(2,2))
plot(trophicLM, which = c(1:3, 5))

par(mfrow = c(2,2))
plot(dwellingLM, which = c(1:3, 5))

TukeyTroph <- TukeyHSD(aov(trophicLM))
print(TukeyTroph)
par(las=1, mar=c(4,10,3,1))
plot(TukeyTroph)

TukeyTroph <- TukeyHSD(aov(trophicLM))
print(TukeyTroph)
par(las=1, mar=c(4,10,3,1))
plot(TukeyTroph)

factorTable <- table(mammals$GroundDwelling, mammals$TrophicLevel)
print(factorTable)
chisq.test(factorTable)

save(mammals, file = 'mammals.Rdata')