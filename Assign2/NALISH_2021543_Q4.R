# All the hypothesis and conclusions have been mentioned in the report

sample1_size <- 9
sample1_mean <- 2
sample1_sd <- sqrt(0.75)

sample2_size <- 16
sample2_mean <- 3.2
sample2_sd <- 1

tstat <- (sample1_mean - sample2_mean) / (sqrt(((sample1_sd^2)/sample1_size) + ((sample2_sd^2)/sample2_size) )) # nolint

t_criticalval_1 <- (qt(p = 0.025, df = 8, lower.tail = TRUE))
t_criticalval_2 <- (qt(p = 0.025, df = 8, lower.tail = FALSE))

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
print(2 * pt(q = tstat, df = 8, lower.tail = TRUE))
print("The average amount of time boys and girls in the given age group playing each day is not same")