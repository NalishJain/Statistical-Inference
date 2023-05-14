# All the hypothesis and conclusions have been mentioned in the report

sample_food_a <- c(49, 53, 51, 52, 47, 50, 52, 53)
sample_food_b <- c(52, 55, 52, 53, 50, 54, 54, 53)

sample_mean <- mean(sample_food_a - sample_food_b)
sample_sd <- sd(sample_food_a - sample_food_b)

mu <- 0
# print(sample_mean)
# print(sample_sd)

tstat <- (sample_mean - mu) / (sample_sd / sqrt(8))
t_criticalval_1 <- (qt(p = 0.025, df = 7, lower.tail = TRUE))
t_criticalval_2 <- (qt(p = 0.025, df = 7, lower.tail = FALSE))

if (tstat > t_criticalval_1 && tstat < t_criticalval_2) {
    print("NULL Accepted")
}else {
    print("Null Rejected")
}
print("Value of the test statistic is : ")
print(tstat)
print("Critical Values are : ")
print(t_criticalval_1)
print(t_criticalval_2)
print("P-value is : ")
print(2 * pt(q = tstat, df = 7, lower.tail = TRUE))
print("There is change in the average weight of babies due to food B")