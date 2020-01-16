#Lab 1 BINF6310 Spring 2020 Jon Lee

#Loaded Die
#Demonstrating the Law of Large Numbers

#Question 1
mean <- (1*(1/10))+(2*(1/10))+(3*(1/10))+(4*(1/10))+(5*(1/10))+(6*(1/2))
  #mean = 4.5
var <- ((1/10)*((1-mean)^2))+((1/10)*((2-mean)^2))+((1/10)*((3-mean)^2))+((1/10)*((4-mean)^2))+((1/10)*((5-mean)^2))+((1/2)*((6-mean)^2))
  #variance = 3.25

#Question 2
rollDie <- function(x)
{
  rolls <- vector(length=x, mode="double")
  
  for(i in 1:x)
  {
    rolls[i] <- sample(1:6, 1, prob=c(0.1, 0.1, 0.1, 0.1, 0.1, 0.5)) 
  }
  
  rolls
}

rollDie(100)

#Question 3
hist(rollDie(10000), main="Histogram of Rolling Loaded Die", xlab="10000 rolls")

#Question 4
trialSizes <- c(5, 10, 15, 20, 25, 30, 40, 50 ,100, 200, 300, 400, 500, 1000, 2000, 3000, 4000, 5000, 10000, 20000, 30000, 100000)
means <- vector(length=length(trialSizes), mode="double")
vars <- vector(length=length(trialSizes), mode="double")

for(i in 1:length(trialSizes))
{
  rolls <- vector(length=trialSizes[i], mode="double")
  
  for(j in 1:trialSizes[i])
  {
    rolls[j] <- sample(1:6, 1, prob=c(0.1, 0.1, 0.1, 0.1, 0.1, 0.5))
  }
  
  means[i] <- mean(rolls)
  vars[i] <- var(rolls)
}

plot(log10(trialSizes), means, main="Mean vs. Increase Trial Sizes", pch=18)
lines(log10(trialSizes), rep(mean, length(trialSizes)))

plot(log10(trialSizes), vars, main="Variance vs. Increase Trial Sizes", pch=18)
lines(log10(trialSizes), rep(var, length(trialSizes)))
