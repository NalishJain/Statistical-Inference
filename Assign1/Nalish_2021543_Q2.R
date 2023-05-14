mydata <- read.csv("data.csv", header = TRUE)
# extracted relevant information from the csv file
birthWeight <- mydata[,2]
# print(birthWeight)

# 2a
# Calculating the log of likelihood function in order to find MLE for normal distribution
# I am estimating standard deviation not variance
myFunction <- function(theta, sample) {
    sampleMean <- theta[1]
    sampleStd <- theta[2]
    n <- length(sample)
    value <- (-n/2*log(2*pi*sampleStd^2) - 1/(2*sampleStd^2)*sum((sample - sampleMean)^2))*(-1)
    return(value)
}

# Using nlminb to calculate the MLE 
answer <- nlminb(start = c(mean(birthWeight), sd(birthWeight)), myFunction, sample = birthWeight)

# Print the estimated parameter values
print("MLE of theta[1]: ")
print(answer$par[1])
print("MLE of theta[2]:")
print(answer$par[2])

# 2b
# Calculating the values of x and y for plotting the graph
mean.values <- seq(1000, 6000, by = 100)
normalSample1ll.nrm <- -1*sapply(mean.values, function(mean) myFunction(c(mean, answer$par[2]), birthWeight))


plot(mean.values, normalSample1ll.nrm , type = "l", xlab = "mean", ylab = "Log-Likelihood",
     main = "Log-Likelihood v/s sigma plot", xlim = c(100, 6000), ylim = c(min(normalSample1ll.nrm ), max(normalSample1ll.nrm )))

#2c
# Using the result mentioned in the report for question3 its MLE estimate will be
print("Answer of 3c is : ")
print(exp(-answer$par[1]))