# All the hypothesis and conclusions have been mentioned in the report

sample <- c(14.3, 12.6, 13.7, 10.9, 13.7, 12.0, 11.4, 12.0, 12.6, 13.1)
sample_mean <- mean(sample)
sample_std <- sd(sample)
mu <- 12

tstat <- (sample_mean - mu) / (sample_std / sqrt(10))
t_criticalval_1 <- (qt(p = 0.025, df = 9, lower.tail = TRUE))
t_criticalval_2 <- (qt(p = 0.025, df = 9, lower.tail = FALSE))
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
print(2 * pt(q = tstat, df = 9, lower.tail = FALSE))
print("The new variety of green gram is expected to give a yield of 12 quintiles per hectare")