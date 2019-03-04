# Statistics Practical 3 - regression  

setwd("H:/Biology/Statistics/Practical 3")
genome <- read.csv('GenomeSize.csv')

pairs(genome, col = genome$Suborder)

# create a small data frame
dat <- data.frame(A = c("a", "b", "c", "d", "e"), B = c(1, 2, 3, 4, 5))
# select row 1 (all columns selected)
dat[1, ]

# select column 2 (all rows selected)
dat[, 2]

# select row 2, column 1
dat[2, 1]

morpho <- c(4, 7, 8, 12, 14)
pairs(genome[, morpho], col = genome$Suborder)

cor(genome[, morpho], use = "pairwise")
cor.test(genome$GenomeSize, genome$TotalLength, use = "pairwise")

genome$logGenomeSize <- log(genome$GenomeSize)
genome$logBodyWeight <- log(genome$BodyWeight)
genome$logTotalLength <- log(genome$TotalLength)
genome$logForewingLength <- log(genome$ForewingLength)
genome$logForewingArea <- log(genome$ForewingArea)

logmorpho <- c(17:21)

pairs(genome[, logmorpho], col = genome$Suborder)

cor(genome[, logmorpho], use = "pairwise")
cor.test(genome$logGenomeSize, genome$logTotalLength, use = "pairwise")

plot(genome$logBodyWeight ~ genome$logGenomeSize, col = genome$Suborder)

nullModelDragon <- lm(logBodyWeight ~ 1, data = genome, subset = Suborder == "Anisoptera")
genomeSizeModelDragon <- lm(logBodyWeight ~ logGenomeSize, data = genome, subset = Suborder == "Anisoptera")
summary(genomeSizeModelDragon)
anova(genomeSizeModelDragon)

genomeSizeModelDamsel <- lm(logBodyWeight ~ logGenomeSize, data = genome, subset = Suborder == "Zygoptera")
summary(genomeSizeModelDamsel)
anova(genomeSizeModelDamsel)

par(mfrow = c(2, 2), mar = c(3, 3, 1.5, 1))
plot(genomeSizeModelDragon, which = c(1, 2))
plot(genomeSizeModelDamsel, which = c(1, 2))

plot(genome$logBodyWeight ~ genome$logGenomeSize, col = c("royalblue", "red")[genome$Suborder])
abline(genomeSizeModelDragon, col = "royalblue")
abline(genomeSizeModelDamsel, col = "red")