spamdata <- read.csv("spam.csv")
names(spamdata) #variable column headings
str(unique(spamdata$spampct)) #view the unique values of spampct
sum(is.na(spamdata$spampct)) # count of NA values in spampct

# split the data 
pctmissing = subset(spamdata, is.na(spampct)) #those where spampct is missing
pctnotmissing = subset(spamdata, !is.na(spampct)) # those where spampct is not missing

# compute the box plots some of the variables
boxplot(pctmissing$time.of.day, pctnotmissing$time.of.day,
names = list("NA pct", "not NA pct"), main = "Comparitive Boxplots")

# empirical CDF
plot(ecdf(pctmissing$time.of.day), verticals=TRUE, pch=46, col = "blue",
     main = "Comparitive Empirical CDF’s")
lines(ecdf(pctnotmissing$time.of.day), verticals=TRUE, pch=46, col = "red")
legend(0, 1, legend=c("Missing", "Not Missing"), lwd=1:3, col = c("blue","red"))

# QQplot
qqplot(pctmissing$time.of.day,pctnotmissing$time.of.day,
       xlab ="Missing",ylab="Not Missing", main = "QQPlot")

# -------------------------------------------------------------------------------------

# Analyis of the iris data base
str(iris)
dim(iris)
names(iris)
attributes(iris)
summary(iris)

# histogram and density estimation
hist(iris$Sepal.Length)
hist(iris$Sepal.Length,breaks="Sturges")
hist(iris$Sepal.Length,breaks="FD")
hist(iris$Sepal.Length,breaks=20)
plot(density(iris$Sepal.Length),col="green",main="combined density")


# summary statistics as a function of species
aggregate(Sepal.Length ~ Species, summary, data=iris)
boxplot(Sepal.Length~Species, data=iris)

# scatter plot as a function of species
plot(iris$Sepal.Length, iris$Sepal.Width, col=iris$Species, pch=as.numeric(iris$Species))

plot(iris$Sepal.Length, iris$Sepal.Width, main="PDF Scatterplot Example", col=rgb(0,100,0,50,maxColorValue=255), pch=16)
require(hexbin)
bin<-hexbin(iris$Sepal.Length, iris$Sepal.Width, xbins=50) 
plot(bin, main="Hexagonal Binning")

# split sceen and plot densities by species
split.screen(figs=c(2, 2))
screen(1)
plot(density(iris$Sepal.Length),col="green",main="combined density")
screen(2)
plot(density(iris$Sepal.Length[iris$Species=="setosa"]),col="red",
     main="Setosa density")
screen(3)
plot(density(iris$Sepal.Length[iris$Species=="virginica"]),col="red",
     main="virginica density")
screen(4)
plot(density(iris$Sepal.Length[iris$Species=="versicolor"]),col="red",
     main="versicolor density")
close.screen(all=TRUE)


# pair-wise scatter plot
pairs(iris[1:4])
# pairwise scatter plot by species
pairs(iris[1:4],pch=22,bg = c("red", "green3", "blue")[unclass(iris$Species)])

# parallel co-ordindate plots
parcoord(iris[1:4], col=iris$Species)

# QQ plots
qqnorm(iris$Sepal.Length)
qqline(iris$Sepal.Length, col = 2)


# Plots by species
library(ggplot2)
qplot(Sepal.Length, Sepal.Width, data=iris, facets=Species ~.)

