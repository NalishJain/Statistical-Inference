# All the hypothesis and conclusions have been mentioned in the report
numberOfBreads <- 10 
Breads.stddeviation <- 0.5
BreadMuNull <- 15
BreadSample.mean <- 17 

breadNormalStat <-(BreadSample.mean -BreadMuNull)/(Breads.stddeviation/sqrt(numberOfBreads)) 

bread_normal_stat_crit <- (qnorm(0.05, mean = 0, sd = 1, lower.tail = FALSE))

if (breadNormalStat < bread_normal_stat_crit) {
    print("NULL Accepted")
}else {
    print("Null Rejected")
}
print("Value of the test statistic is : ")
print(breadNormalStat)
print("Critical Values is : ")
print(bread_normal_stat_crit)
print("p-value is : ")
print(pnorm(breadNormalStat, mean = 0, sd = 1, lower.tail = FALSE))
print("The average size of a bread is greater than 15 cm")