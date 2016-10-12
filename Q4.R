rm(list=ls(all=TRUE))
   
   
# Create Simulated data
set.seed(1)
x <- rnorm(100)
x
eps <- rnorm(100, mean=0, sd=sqrt(1))
eps
y <- -1 + 0.5*x+eps

# Scatter plot
plot(x,y, title(main="Scatterplot: x vs y"))

# Linear Regression Model
lm.fit=lm(y~x)
summary(lm.fit)

# Let's look at the scatter plot
plot(x,y, title(main="Scatterplot: x vs y, Least Squares and Population Lines") )
lineLM <- abline(lm(y~x), col="red")
linePop <- abline(a=-1, b=0.5, col="blue")
legend("topleft", legend = c("Least Squares", "Population", "Sample"), lwd=1, pch = 1, col=c("red", "blue", "black"))

# Fitting a polynomial regression
lm.fit2=lm(y~x+I(x^2))
summary(lm.fit2)
summary(lm.fit)
confint(lm.fit)
