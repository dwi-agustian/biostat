#Explaratory Data analysis
names(fHH1)

#check the mean and variance
mean(fHH1$total)
var(fHH1$total)

# test statistics: H0 : Mean - Var = 0
mean_val <- mean(fHH1$total)
var_val <- var(fHH1$total)
n <- length(fHH1$total)

# Estimate standard errors for mean and variance
se_mean <- sd(fHH1$total) / sqrt(n)
se_var <- var(fHH1$total) * sqrt(2 / (n - 1)) # Approximate SE for variance

# Test statistic
t_stat <- (mean_val - var_val) / sqrt(se_mean^2 + se_var^2)
p_value <- 2 * pt(-abs(t_stat), df = n - 1)

print(t_stat)
print(p_value)
