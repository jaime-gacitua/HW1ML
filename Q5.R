# clean up
rm(list=ls(all=TRUE))

allB1 = numeric(length=100)
mcarB1 = numeric(length=100)
marB1 = numeric(length=100)
mnarB1 = numeric(length=100)

allB0 = numeric(length=100)
mcarB0 = numeric(length=000)
marB0 = numeric(length=100)
mnarB0 = numeric(length=100)


# parameters
sigma <- matrix(c(25^2, 0.6*25^2, 0.6*25^2, 25^2), 2, 2)
mu <- c(125,125)

for (t in 1:100 ){
  # generate bivariate normal random variable and then missing values
  values <- data.frame(mvrnorm(50, mu, sigma))
  mcar <- data.frame(values[sample(nrow(values), size=14, replace=FALSE),])
  mar <- subset(values, X1 > 140)
  mnar <- subset(values, X2 > 140)
  
  # fit y and x
  lm.fit <- lm(values[,2]~values[,1])
  lm.fit.mcar <- lm(mcar[,2]~mcar[,1])
  lm.fit.mar <- lm(mar[,2]~mar[,1])
  lm.fit.mnar <- lm(mnar[,2]~mnar[,1])

  # record slopes  
  allB1[t] <- summary(lm.fit)$coefficients[[2,1]]
  mcarB1[t] <- summary(lm.fit.mcar)$coefficients[[2,1]]
  marB1[t] <- summary(lm.fit.mar)$coefficients[[2,1]]
  mnarB1[t] <- summary(lm.fit.mnar)$coefficients[[2,1]]

  # record intercepts  
  allB0[t] <- summary(lm.fit)$coefficients[[1,1]]
  mcarB0[t] <- summary(lm.fit.mcar)$coefficients[[1,1]]
  marB0[t] <- summary(lm.fit.mar)$coefficients[[1,1]]
  mnarB0[t] <- summary(lm.fit.mnar)$coefficients[[1,1]]
  

  }

summary(lm.fit)

plot(values[,1], values[,2], main="Linear Regression with Missing Values", xlab="Month 1", ylab = "Month 2")
line <- abline(lm.fit, col="black")
lineMCAR <- abline(lm.fit.mcar, col="blue", lty=2)
lineMAR <- abline(lm.fit.mar, col="brown", lty=3 )
lineMNAR <- abline(lm.fit.mnar, col="red", lty=4)
legend("bottomright", legend = c("All Values", "MCAR", "MAR", "MNAR"), lty=c(1,2,3,4), 
       col=c("black", "blue", "brown", "red"))




avgCoef <- matrix(c("all data", "MCAR", "MAR", "MNAR", 
                  mean(allB0), mean(mcarB0), mean(marB0), mean(mnarB0), 
                  mean(allB1), mean(mcarB1), mean(marB1), mean(mnarB1)), 
                nrow = 4, ncol=3)
avgCoef
  
muY.mcar <- mean(mcarB0)+mean(mcarB1)*mean(values[,1])
muY.mar <- mean(marB0)+mean(marB1)*mean(values[,1])
muY.mnar <- mean(mnarB0)+mean(mnarB1)*mean(values[,1])

c(muY.mcar, muY.mar, muY.mnar)

