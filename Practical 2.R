# Using a T test to test if mean variable between dragonflies and damselflies are differnt and an F test to check if they have the same variance

genome <- read.csv('GenomeSize.csv')
str(genome)

# calculate the three values from the data
mean.gs <- mean(genome$GenomeSize)
print(mean.gs)

var.gs <- var(genome$GenomeSize)
print(var.gs)

n.gs <- length(genome$GenomeSize)
print(n.gs)

# get the difference
diff <- mean.gs - 1.25
print(diff)

# get the standard error
se.gs <- sqrt(var.gs/n.gs)
print(se.gs)

# get the t value
t.gs <- diff/se.gs  # t = diffence/standard error
print(t.gs)

t.test(genome$GenomeSize, mu = 1.25)  #mu=mean

# Find the edges of the middle 95% of a $t$ distribution with 99 df
# (quantiles of the t distribution, so qt, the quantile function)
tlim <- qt(c(0.025,0.975), df = 99)
print(tlim)

# use the mean and standard error from above to get a confidence interval
mean.gs + tlim * se.gs


t.test(genome$BodyWeight, mu = 0.045)  #mu=mean


# calculate the three values from the data
mean.gs <- tapply(X = genome$GenomeSize, INDEX = genome$Suborder, FUN = mean)
print(mean.gs)

var.gs <- tapply(X = genome$GenomeSize, INDEX = genome$Suborder, FUN = var)
print(var.gs)

n.gs <- tapply(X = genome$GenomeSize, INDEX = genome$Suborder, FUN = length)
print(n.gs)

# get the difference
diff <- mean.gs[1] - mean.gs[2]
print(diff)

# get the standard error of the difference
se.gs <- sqrt((var.gs[1]/n.gs[1]) + (var.gs[2]/n.gs[2]))
print(se.gs)

# get the t value
t.gs <- diff/se.gs
print(t.gs)

# two smaple t-test for difference in genome size
t.test(GenomeSize ~ Suborder, data = genome)

# two smaple t-test for difference in body weight
t.test(BodyWeight ~ Suborder, data = genome)

par(mfrow = c(1, 2)) 
plot(GenomeSize ~ Suborder , data = genome)
plot(BodyWeight ~ Suborder , data = genome)

var.gs[1]/var.gs[2]
var.test(GenomeSize ~ Suborder, data = genome)
var.test(BodyWeight ~ Suborder, data = genome)

genome$logBodyWeight <- log(genome$BodyWeight)
plot(logBodyWeight ~ Suborder , data = genome)
var.test(logBodyWeight ~ Suborder, data = genome)
t.test(logBodyWeight ~ Suborder, data = genome)

# Wilcox test
wilcox.test(genome$GenomeSize, mu = 1.25)
wilcox.test(GenomeSize ~ Suborder, data = genome)

wilcox.test(genome$BodyWeight, mu = 0.045)
wilcox.test(BodyWeight ~ Suborder, data = genome)