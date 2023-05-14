# All the hypothesis and conclusions have been mentioned in the report

mu <- 15
sample_mean <- 17.4
sample_std <- 6.3
n <- 75

tstat <- (sample_mean - mu) / (sample_std / sqrt(n))
t_criticalval_1 <- (qt(p = 0.025, df = 74, lower.tail = TRUE))
t_criticalval_2 <- (qt(p = 0.025, df = 74, lower.tail = FALSE))

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
print(2 * pt(q = tstat, df = 74, lower.tail = FALSE))
print("Population mean time on death row won't be 15 years")