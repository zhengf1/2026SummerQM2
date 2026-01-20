# ===============================
# Load and View Data
# ===============================

# Read in data
# Either use File -> Import Dataset as instructed in previous tutorials or
# use code below by editing the file paths to match your computer

library(readxl)  # Load the readxl package for reading Excel files

# Load datasets for Exercises 1-4
setwd("~/Dropbox/01 UoM-Teaching/2026-Summer-QM2/Tute5")
t5e1 <- read_excel("t5e1.xlsx")
 
t5e2 <- read_excel("t5e2.xlsx")
 
t5e3 <- read_excel("t5e3.xlsx")
 
t5e4 <- read_excel("t5e4.xlsx")
 
# ===============================
# Exercise 1: One-sample variance test
# ===============================

# (a) CI

cv1 = qchisq(0.05, 9, lower.tail = FALSE)
cv2 = qchisq(0.95, 9, lower.tail = FALSE)

n = dim(t5e1)[1]
s = sd(t5e1$length)

CI_lower = (n-1) * s^2 / cv1
CI_upper = (n-1) * s^2 / cv2

c(CI_lower, CI_upper)

# (b) test
null_reference = 0.4
chi_test_statistics = (n-1) * s^2 / null_reference

# Install DescTools package if not already installed
# install.packages("DescTools")

library(DescTools)

# Test if variance of 'length' is significantly greater than 0.4
VarTest(t5e1$length, sigma.squared = 0.4, alternative = "greater")

# Two-sided test to check if variance differs from 0.4
VarTest(t5e1$length, sigma.squared = 0.4)


# ===============================
# Exercise 2: Two-sample variance comparison
# ===============================

# (a) CI
n1 = length(t5e2$Teller1)
n2 = length(t5e2$Teller2)

df1 = n1-1
df2 = n2-1

cv1 = qf(0.025,df1,df2,lower.tail = FALSE)
cv2 = qf(0.975,df1,df2,lower.tail = FALSE)
# or
cv2 = 1/cv1


CI_lower = (sd(t5e2$Teller1)^2) / (sd(t5e2$Teller2)^2) /cv1
CI_upper = (sd(t5e2$Teller1)^2) / (sd(t5e2$Teller2)^2) /cv2

c(CI_lower, CI_upper)

# (b) test
# now 10% (not 5%)
cv = qf(0.05,df1,df2,lower.tail = FALSE)

F_test_statistics = (sd(t5e2$Teller1)^2) / (sd(t5e2$Teller2)^2)



library(DescTools)

# Test equality of variances between Teller1 and Teller2
VarTest(t5e2$Teller1, t5e2$Teller2)

# --- Visual inspection of normality for each Teller ---

# Histogram and normal curve for Teller1
hist(t5e2$Teller1, freq = FALSE, col = "blue")
lines(seq(0, 16, by = 0.1),
      dnorm(seq(0, 16, by = 0.1), mean(t5e2$Teller1), sd(t5e2$Teller1)),
      col = "red")

# Histogram and normal curve for Teller2
hist(t5e2$Teller2, freq = FALSE, col = "green")
lines(seq(0, 16, by = 0.1),
      dnorm(seq(0, 16, by = 0.1), mean(t5e2$Teller2), sd(t5e2$Teller2)),
      col = "red")

# Q-Q plot for Teller1
qqnorm(t5e2$Teller1, main = "Normal Q-Q Plot for Teller1",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", col = "blue")
qqline(t5e2$Teller1, col = "red")

# Q-Q plot for Teller2
qqnorm(t5e2$Teller2, main = "Normal Q-Q Plot for Teller2",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", col = "green")
qqline(t5e2$Teller2, col = "red")

# --- Summary statistics for each Teller ---

library(pastecs)

# Teller1 stats including skewness, kurtosis, and normality test
round(stat.desc(t5e2$Teller1, basic = FALSE, desc = TRUE, norm = TRUE), 3)

# Teller2 stats
round(stat.desc(t5e2$Teller2, basic = FALSE, desc = TRUE, norm = TRUE), 3)


# ===============================
# Exercise 3: One-sample proportion tests
# ===============================

# Frequency table of "used" responses
table(t5e3$used)

# Relative frequency as a proportion
table(t5e3$used) / length(t5e3$used)

# Alternative way to calculate proportions
prop.table(table(t5e3$used))

# calculate np and nq
n3 = length(t5e3$used)
n3*prop.table(table(t5e3$used))
# both above 5

# Binomial test: 126 out of 600 successes, 99% CI
binom.test(126, 600, conf.level = 0.99)

# or using formula 
# phat +- z_cv * se(phat)
phat = prop.table(table(t5e3$used))[2]
z_cv = qnorm(0.995)
se_phat = sqrt(phat*(1-phat)/n3)
c(phat - z_cv * se_phat, phat + z_cv * se_phat)

# (b) hypothesis testing 
# One-sided binomial test (is p > 0.2?) with 95% CI
binom.test(126, 600, p = 0.2, alternative = "greater", conf.level = 0.95)

# Proportion z-test (approximate method)
prop.test(126, 600, p = 0.2, alternative = "greater", conf.level = 0.95)


# ===============================
# Exercise 4: Two-sample proportion test
# ===============================

# Frequencies for each sample
table(t5e4$X1)
table(t5e4$X2)

# use formula (make sure you can do this)
n1 = sum(table(t5e4$X1))
p1 = table(t5e4$X1)[2] / n1 
n2 = sum(table(t5e4$X2))
p2 = table(t5e4$X2)[2] / n2
se_p1_p2 = sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)
# CI 90%
p1 - p2 - se_p1_p2 * qnorm(0.95)
p1 - p2 + se_p1_p2 * qnorm(0.95)

# Two-sample test for difference in proportions, 90% CI
prop.test(x = c(248, 260), n = c(400, 500), conf.level = 0.90)

# Same test, but without continuity correction
prop.test(x = c(248, 260), n = c(400, 500), conf.level = 0.90, correct = FALSE)
# (without correction) would be similar to our hand calculation


# (b) hypothesis testing 10%
# One-sided test (is group 1 proportion > group 2?), no correction
prop.test(x = c(248, 260), n = c(400, 500),
          alternative = "greater", conf.level = 0.90
          , correct = FALSE)

p_agg = (248 + 260) / 900
se_agg = sqrt(p_agg*(1-p_agg)*(1/n1 + 1/n2))
test = (p1-p2) / se_agg
if(test >= qnorm(0.9)) cat("reject") else cat("fail to reject")


# One-sided test with correction
prop.test(x = c(248, 260), n = c(400, 500),
          alternative = "greater", conf.level = 0.90)
