library(ape)
library(caper)
library(geiger)

setwd("downloads/Practical 2")
mammal.orders <- read.delim("MammalOrderS.txt")

source("hcd.functions.R")
mammal.hcd <- hcd.fit(mammal.orders$richness, reps = 1)
plot.hcd(mammal.hcd)  
# Red line shows mammal richness, black line shows equal rates Markov model

mammal.hcd <- hcd.fit(mammal.orders$richness, reps = 1000, minmax = TRUE)
plot.hcd(mammal.hcd)
# 1000 repititions, finding the range of possbile results for ERM model 

# Store P value
p.value <- mammal.hcd$num.hi/mammal.hcd$reps 
print(p.value)

# Create aphylogenetic tree
mammal.tree <- read.tree("mammals.tre")
plot(mammal.tree)

# get the imbalance score for the phylogeny, test of phylogentic imbalance 
mammal.imbalance <- fusco.test(phy = mammal.tree, dat = mammal.orders,
                               rich = richness, names.col = order)
summary(mammal.imbalance)
plot(mammal.imbalance)  # Plot of imblance scores
# Black line is observed mean I' (I prime is imblance score)
# Red line is the 95% confidence intervals of null distribution

# Q1: That the ERM model does not accuratly predict imblance score of mammal tree
# ans: That ERM is not the model under which mammals diversified, but that different clades have had
# different chances of diversifying for some reason.

# Q2: That the imbalnce score is statisticaly significantly differnt from prdiction of I'=0.5
# ans: That ERM is not the model under which mammals diversified, but that different clades have had
# different chances of diversifying for some reason.

# function to calculate a Slowinski Guyer p value
# given the species richness of two sister taxa
sg.test <- function(n1, n2){
s <- min(n1, n2)  # Assigns s the smaller of the two numbers
N <- n1 + n2      # Assigns N the sum of the two numbers
p <- 2*s/(N-1)    #Applies the equation to compute p
if (p<1) {
  return(p)
  } else {
    return(1)
  }
}

sg.test(1, 25)   # p=0.08 therfore does NOT reject ERM model
sg.test(1, 50)   # p=0.04 therfore DOES reject ERM model
sg.test(2, 70)   # p=0.0563 therfore does NOT reject ERM model
sg.test(50, 50)  # P value exceeds 1, error with function 

# Craete a plot of lineages through time, 
# shows the species accumulation, part of diversification
erm <- growTree(b = 1, d = 0, halt = 58)  # simulate clade growth specaiton only ERM model
par(mfrow = c(1, 2))  # Create two side-by-side plots
plot(erm$phy, cex = 0.7)
ltt.plot(erm$phy, log = "y")
# simulation so there is randomness in the simulated plots

# using read data from the phylloscopus genus, instead of simulated
phylloscopus <- read.nexus("phylloscopus.nex")
par(mfrow = c(1, 2))  # Create two side-by-side plots
plot(phylloscopus, cex = 0.7)
ltt.plot(phylloscopus, log = "y")

gammaStat(phylloscopus)  # -5.338684 < 1.68 hence diversification has sigificantly slowed down

# Simulation that includes extinction as well
erm <- growTree(b = 1, d = 0.5, halt = 500)  # d=0.5, species=500
alive <- drop.extinct(erm$phy)
par(mfrow = c(1, 2))  # Create two side-by-side plots
plot(alive, cex = 0.7)
ltt.plot(alive, log = "y")

gammaStat(alive)  # extinciton means that there is space for speciaiton to occur & more species= higher probability of speciaiton

# Q3. Which one or more of the following are features of the equal-rates Markov (ERM) model?
# 1. A constant number of species in the clade
# 2. A constant per-lineage extinction rate
# 3. A constant total overall speciation rate
# 4. A constant per-lineage speciation rate
# 5. Density-dependence in speciation
# 6. Rates of diversification depend on traits of the species
# ans: 2, 4

# Q4. Which one or more of the following assumptions did you make when using hcd.fit to test ERM?
# 1. All taxa had equal numbers of species
# 2. All taxa were paraphyletic
# 3. All taxa were monophyletic
# 4. All taxa were the same age
# 5. All data were very old
# 6. No taxa were very old
# ans: 3, 4

# Q5: According to ERM, why are there not always equal numbers of species in two sister clades?
# 1. One sister clade is usually luckier than the other, just by chance, so has more species
# 2. One sister clade is usually older than the other, so has more species
# 3. One sister clade is usually more competitive than the other, so has more species
# ans: 1

# Q6: What does a mean I0 significantly greater than 0.5 indicate?
# 1. That each lineage has a lot of species
# 2. That all clades have had the same chances of diversifying
# 3. That all clades have had different chances of diversifying
# 4. That at least some clades have had different chances of diversifying
# 5. That small-bodied clades are the most diverse
# 6. That the observed mean I0 is larger than in nearly all of the randomisations
# ans: 4

# Q7: How might you be able to tell that a clade's diversification had slowed down signifcantly
# 1. Successive nodes in phylogeny would get closer and closer as you get nearer to the tips through time?
# 2. Successive nodes in phylogeny would get further apart as you get nearer to the tips
# 3. The gamma statistic would be less than -1.68
# 4. The gamma statistic would be less than 0
# 5. The gamma statistic would be greater than -1.68
# ans: 2, 3










