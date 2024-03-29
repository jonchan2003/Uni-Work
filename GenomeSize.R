# Linear Modelling Practical One

# This script uses genome size and morphology data to explore 
# data frames and plotting in R


# (1) load the data from the CSV text file into a data frame

genome <- read.csv('GenomeSize.csv')

# (2) check the data in the the new data frame
# - str(X) shows the structure of the data named X
# - head(X) shows the first few lines of X

str(genome)
head(genome)

# (3) look at a summary of all the data

summary(genome)

# (4) Visualising a distribution

hist(genome$GenomeSize, breaks=10)
plot(density(genome$GenomeSize, bw=0.1))

# (5) Comparing two distributions using boxplots

plot(GenomeSize ~ Suborder, data=genome)

# (6) Comparing two distributions using density plots
# - first, get two small datasets, one for each order.

Anisoptera <- subset(genome, Suborder=='Anisoptera')
Zygoptera <- subset(genome, Suborder=='Zygoptera')

# - now plot the first suborder and add a line for the second
# - the limits of the plot need to be adjusted to fit both curves

plot(density(Zygoptera$GenomeSize), xlim=c(0.1, 2.7), ylim=c(0,1.7))
lines(density(Anisoptera$GenomeSize), col='red')

# (7) Scatterplots - plotting one variable against another

plot(TotalLength ~ GenomeSize , data=genome)

# (8) Customizing scatterplots 

myColours <- c('red', 'blue')
mySymbols <- c(1,3)

str(genome$Suborder)

plot(TotalLength ~ GenomeSize , data=genome, 
     col=myColours[Suborder], pch=mySymbols[Suborder])

legend(2,40, legend=levels(genome$Suborder), pch= mySymbols, col= myColours)

# (9) Saving a file as a pdf 
# - this code opens a new pdf file and plots the scatterplot in it
# - the command dev.off() closes the pdf file

pdf('GenomeSize.pdf', height=5, width=6)

	plot(TotalLength ~ GenomeSize , data=genome, 
	     col=myColours[Suborder], pch=mySymbols[Suborder],
	     ylab='Total length (mm)', xlab='Genome size (pg)')

dev.off()

# (10) Save the data in R format
save(genome, myColours, mySymbols, file='GenomeSize.Rda')
