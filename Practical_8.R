library(lattice)

setwd("H://Biology//Statistics//Practical 8")
lizards <- read.csv("lizard_niches.csv")
boxplot(Count ~ species, data = lizards)
lizards$logCount <- log(lizards$Count + 1)
boxplot(logCount ~ species, data=lizards)

tapply(lizards$Count, lizards$species, min, na.rm = TRUE)
tapply(lizards$Count, lizards$species, max, na.rm = TRUE)
lizardsubset <- subset(lizards, species %in% c("grahamii", "lineatopus"))
str(lizardsubset)
lizardsubset <- na.omit(lizardsubset)
lizardsubset <- droplevels(lizardsubset)
str(lizardsubset)

bwplot(logCount ~ height | species, data = lizardsubset)
tab <- tapply(lizardsubset$Count, list(lizardsubset$species, lizardsubset$height), mean, na.rm = TRUE)
print(tab)
barplot(tab, beside = TRUE, log = 'y')

modLM <- lm(logCount ~ height * species, data = lizardsubset)
par(mfrow = c(2, 2), mar = c(3, 3, 3, 1), mgp = c(2, 0.8, 0))
plot(modLM)

modPois <- glm(Count ~ height * species, data = lizardsubset, family = 'poisson')
summary(modPois)
par(mfrow = c(2, 2), mar = c(3, 3, 3, 1), mgp = c(2, 0.8, 0))
plot(modPois)

modQPois <- glm(Count ~ height * species, data = lizardsubset, family = 'quasipoisson')
summary(modQPois)
par(mfrow = c(2, 2), mar = c(3, 3, 3, 1), mgp = c(2, 0.8, 0))
plot(modQPois)
anova(modQPois, test = "F")
drop.scope(modQPois)
modQPois2 <- update(modQPois, . ~ . - height:species)
anova(modQPois, modQPois2, test = "F")
df <- expand.grid(height = levels(lizardsubset$height), species = levels(lizardsubset$species))
predict(modQPois, newdata = df, type = "response")