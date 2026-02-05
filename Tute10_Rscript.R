#Install Packages (if not aready installed)
install.packages(c("sandwich","pastecs", "Hmisc", "car", "lmtest"))


setwd("~/Dropbox/01 UoM-Teaching/2026-Summer-QM2/Tute10")

# Read in data either via File->Import Dataset and follow instruction concerning wide and long format or using code below (you will need to end the relevant disk address)
library(readxl)
t10e2 <- read_excel("t10e2.xlsx")
t10e3 <- read_excel("t10e3.xlsx")
t10e4 <- read_excel("t10e4.xlsx")
t10e6 <- read_excel("t10e6.xlsx")
t10e7 <- read_excel("t10e7.xlsx")

# Exercise 2
# Creating dummy variables for Drug A and Drug B
t10e2$D1 <- ifelse(t10e2$Drug == "A", 1, 0)
t10e2$D2 <- ifelse(t10e2$Drug == "B", 1, 0)

# Model 1: Linear regression model without interaction terms
m1 <- lm(Effect ~ Age + D1 + D2, data = t10e2) 
summary(m1)

# Model 2: Linear regression model with interaction terms
m2 <- lm(Effect ~ Age + D1 + D1*Age + D2 + D2*Age, data = t10e2) 
summary(m2) 

# Generating values for age
age <- 0:100

# Calculating estimated effects for each drug
effect_A <- 47.515 + 0.330 * age
effect_B <- 28.169 + 0.544 * age
effect_C <- 6.211 + 1.033 * age

# Plotting estimated drug effectiveness over age
plot(age, effect_C, type = "l", col = "red", lwd = 2, xlab = "Age", ylab = "Estimated effect")
lines(age, effect_A, col = "blue", lwd = 2)
lines(age, effect_B, col = "green", lwd = 2)
title("Estimated drug effectiveness")
legend("topleft", c("Drug A", "Drug B", "Drug C"), 
       lwd = c(2, 2, 2), col = c("blue", "green", "red"))

# Exercise 3

# Fitting a linear regression model
m <- lm(Absent ~ Wage + PctPT + PctU + AvShift + UMRel, data = t10e3)
summary(m) 

# Calculating residuals
res <- residuals(m) 

# Plotting histogram of residuals
hist(res, freq = FALSE, ylim = c(0, 0.2), col = "lightblue") 
lines(seq(-6, 8, by = 0.05),  
      dnorm(seq(-6, 8, by = 0.05), mean(res), sd(res)), 
      col = "red") 

# Creating a normal Q-Q plot
qqnorm(res, main = "Normal Q-Q Plot", 
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", 
       pch = 19, col = "lightgreen")
qqline(res, col = "royalblue4")

# Descriptive statistics of residuals
library(pastecs) 
round(stat.desc(res, basic = FALSE, norm = TRUE), 4) 

# Pearson correlation matrix
library(Hmisc)
rcorr(as.matrix(t10e4), type = "pearson")

# Variance inflation factors
library(car)
round(vif(m), 4) 

# Plot of OLS residuals versus fitted values
yhat <- fitted.values(m)
plot(yhat, res,
     main = "OLS residuals versus yhat",
     col = "red", pch = 19, cex = 0.75) 

# Breusch-Pagan test for heteroscedasticity
library(lmtest)
bptest(m, ~ Wage + PctPT + PctU + AvShift + UMRel +
         I(Wage^2) + I(PctPT^2) + I(PctU^2) + I(AvShift^2) + I(UMRel^2) +
         I(Wage * PctPT) + I(Wage * PctU) + I(Wage * AvShift) + I(Wage * UMRel) +
         I(PctPT * PctU) + I(PctPT * AvShift) + I(PctPT * UMRel) +
         I(PctU * AvShift) + I(PctU * UMRel) + I(AvShift * UMRel), data = t10e3) 

# Exercise 4

# Fitting a linear regression model
lpm <- lm(COKE ~ PRATIO + DISP_COKE + DISP_PEPSI, data = t10e4)
summary(lpm) 

# Calculating fitted values and cross-tabulation with original COKE values
COKE_lpm <- round(fitted.values(lpm), digits = 0)
prop.table(table(t10e4$COKE, COKE_lpm)) 

# Counting negative and values greater than 1 in fitted values
sum(fitted.values(lpm) < 0)
sum(fitted.values(lpm) > 1) 

# Predicting new data
new_data <- data.frame(PRATIO = mean(t10e4$PRATIO), 
                       DISP_COKE = c(1, 0), DISP_PEPSI = c(0, 1)) 
predict(lpm, newdata = new_data, interval = "none") 

# Breusch-Pagan test for heteroscedasticity
library(lmtest)
bptest(lpm, ~ PRATIO + DISP_COKE + DISP_PEPSI +
         I(PRATIO^2) + I(DISP_COKE^2) + I(DISP_PEPSI^2) +
         I(PRATIO * DISP_COKE) + I(PRATIO * DISP_PEPSI) +
         I(DISP_COKE * DISP_PEPSI), data = t10e4) 

# Heteroscedasticity-consistent standard errors
library(sandwich)
coeftest(lpm, vcov = vcovHC(lpm, type = "HC"))

