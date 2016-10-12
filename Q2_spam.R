spamdata <- read.csv("spam.csv")
names(spamdata) #variable column headings
summary(spamdata)

# split the data 
pctmissing = subset(spamdata, is.na(spampct)) #those where spampct is missing
pctnotmissing = subset(spamdata, !is.na(spampct)) # those where spampct is not missing

# Scatter matrix plot
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.6, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.4, txt2)
}
pairs(spamdata, upper.panel = panel.cor)
pairs(pctnotmissing, upper.panel = panel.cor)


# compute the box plots some of the variables
boxplot(pctmissing$time.of.day, pctnotmissing$time.of.day, names = list("NA pct", "not NA pct"), 
        main = "Comparative Boxplots - Time of Day")

summary(pctmissing$time.of.day)
summary(pctnotmissing$time.of.day)


# empirical CDF
plot(ecdf(pctmissing$time.of.day), verticals=TRUE, pch=46, col = "blue",
     main = "Comparitive Empirical CDF - Time of Day")
lines(ecdf(pctnotmissing$time.of.day), verticals=TRUE, pch=46, col = "red")
legend(0, 1, legend=c("Missing", "Not Missing"), lwd=1:3, col = c("blue","red"))


# QQplot
qqplot(pctmissing$time.of.day,pctnotmissing$time.of.day,
       xlab ="Missing",ylab="Not Missing", main = "QQPlot")

plot(pctnotmissing$time.of.day, pctnotmissing$spampct, main="Scatter Plot")

require(hexbin)
bin<-hexbin(pctnotmissing$time.of.day, pctnotmissing$spampct, xbins=50) 
plot(bin, main="Hexagonal Binning")


# -------------------------------------------------------------------------------------

