SampleSize <- 1000

# 1a) 
# Stimulated exponential samples for different values of lambda 
randomSample1 <- rexp(SampleSize, rate = 1)
randomSample2 <- rexp(SampleSize, rate = 2)
randomSample3 <- rexp(SampleSize, rate = 3)
randomSample4 <- rexp(SampleSize, rate = 4)

# 1b)
# Created a function for calculating the log of the likelihood function
myFunction <- function(lambda, sample) {
  n <- length(sample)
  llik <- n*log(lambda) - lambda*sum(sample)
  return(-llik) # Returned the negative log likelihood for optimization
}


# Used the nlminb function to find the MLE of the rate parameter lambda
# Case1 initial value 0.1
print("Case1 ")
randomSample1.exp <- nlminb(start = 0.1, objective = myFunction, hessian = TRUE, lower = 0, upper = Inf, sample = randomSample1)
print(randomSample1.exp)
randomSample2.exp <- nlminb(start = 0.1, objective = myFunction, hessian = TRUE, lower = 0, upper = Inf, sample = randomSample2)
print(randomSample2.exp)
randomSample3.exp <- nlminb(start = 0.1, objective = myFunction, hessian = TRUE, lower = 0, upper = Inf, sample = randomSample3)
print(randomSample3.exp)
randomSample4.exp <- nlminb(start = 0.1, objective = myFunction, hessian = TRUE, lower = 0, upper = Inf, sample = randomSample4)
print(randomSample4.exp)
print("Case2")
# Case2 initial value 9
randomSample1.exp <- nlminb(start = 9, objective = myFunction, hessian = TRUE, lower = 0, upper = Inf, sample = randomSample1)
print(randomSample1.exp)
randomSample2.exp <- nlminb(start = 9, objective = myFunction, hessian = TRUE, lower = 0, upper = Inf, sample = randomSample2)
print(randomSample2.exp)
randomSample3.exp <- nlminb(start = 9, objective = myFunction, hessian = TRUE, lower = 0, upper = Inf, sample = randomSample3)
print(randomSample3.exp)
randomSample4.exp <- nlminb(start = 9, objective = myFunction, hessian = TRUE, lower = 0, upper = Inf, sample = randomSample4)
print(randomSample4.exp)

print("Case3")
randomSample1.exp <- nlminb(start = 1/(mean(randomSample1)), objective = myFunction, hessian = TRUE, lower = 0, upper = Inf, sample = randomSample1)
print(randomSample1.exp)
randomSample2.exp <- nlminb(start = 1/(mean(randomSample2)) , objective = myFunction, hessian = TRUE, lower = 0, upper = Inf, sample = randomSample2)
print(randomSample2.exp)
randomSample3.exp <- nlminb(start = 1/(mean(randomSample3)), objective = myFunction, hessian = TRUE, lower = 0, upper = Inf, sample = randomSample3)
print(randomSample3.exp)
randomSample4.exp <- nlminb(start =1/(mean(randomSample4)) , objective = myFunction, hessian = TRUE, lower = 0, upper = Inf, sample = randomSample4)
print(randomSample4.exp)


# created a vector of x values for plotting the graph

# 1c)
rate.values <- seq(0.1, 5, by = 0.01)

# Calculated the corresponding values for y values
randomSample1ll.exp <- -1*sapply(rate.values, myFunction, sample = randomSample1 )
randomSample2ll.exp <- -1*sapply(rate.values, myFunction, sample = randomSample2 )
randomSample3ll.exp <- -1*sapply(rate.values, myFunction, sample = randomSample3 )
randomSample4ll.exp <- -1*sapply(rate.values, myFunction, sample = randomSample4 )
# Plotted the log likelihood function against the lambda values

par(mfrow = c(2, 2))
plot(rate.values, randomSample1ll.exp, type = "l", xlab = "rate", ylab = "Log-Likelihood",
     main = "Log-Likelihood v/s rate plot", xlim = c(0.1, 5), ylim = c(min(randomSample1ll.exp), max(randomSample1ll.exp)))
plot(rate.values, randomSample2ll.exp, type = "l", xlab = "rate", ylab = "Log-Likelihood",
     main = "Log-Likelihood2 v/s rate plot", xlim = c(0.1, 5), ylim = c(min(randomSample2ll.exp), max(randomSample2ll.exp)))
plot(rate.values, randomSample3ll.exp, type = "l", xlab = "rate", ylab = "Log-Likelihood",
     main = "Log-Likelihood3 v/s rate plot", xlim = c(0.1, 5), ylim = c(min(randomSample3ll.exp), max(randomSample3ll.exp)))
plot(rate.values, randomSample4ll.exp, type = "l", xlab = "rate", ylab = "Log-Likelihood",
     main = "Log-Likelihood4 v/s rate plot", xlim = c(0.1, 5), ylim = c(min(randomSample4ll.exp), max(randomSample4ll.exp)))

