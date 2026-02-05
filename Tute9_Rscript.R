# ===============================
# Load and View Data
# ===============================

# Read in data
# Either use File -> Import Dataset and follow instructions,
# or use the code below by updating the file paths to match your system.

library(readxl)  # For reading Excel files

# Load datasets
t9e1 <- read_excel("t9e1.xlsx")
View(t9e1)

t9e3 <- read_excel("C:/your/path/here/t9e3.xlsx")  # Not used in this script, possibly for other exercises
View(t9e3)


# ===============================
# Exercise 1: Residual diagnostics and robust standard errors
# ===============================

# --- Fit a simple linear regression model ---
# Regress son's height on father's height
m = lm(t9e1$Son ~ t9e1$Father)
summary(m)

# --- Residual analysis ---
# Extract and plot OLS residuals against the predictor
olsres = residuals(m)
plot(t9e1$Father, olsres,
     main = "Scatterplot of OLS Residuals vs Father",
     xlab = "Father's Height", ylab = "Residuals",
     col = "orange", pch = 19)

# --- Breusch-Pagan test for heteroskedasticity ---
library(lmtest)

# Test for non-constant variance of residuals using both linear and quadratic terms
bptest(m, ~ t9e1$Father + I(t9e1$Father^2))

# --- Robust standard errors ---
library(sandwich)

# HC0 (classical White) heteroskedasticity-consistent standard errors
coeftest(m, vcov = vcovHC(m, type = "const"))

# HC1-4 variants; HC is the default robust estimator used in most textbooks
coeftest(m, vcov = vcovHC(m, type = "HC"))


# ===============================
# Exercise 2: Prediction and intervals
# ===============================

# --- Re-specify model using formula and data argument (equivalent to Exercise 1) ---
m = lm(Son ~ Father, data = t9e1)
summary(m)

# --- Create new observation for prediction ---
newdata = data.frame(Father = 175)

# --- 90% prediction interval for a new observation (individual-level uncertainty) ---
predict(m, newdata, level = 0.90, interval = "prediction")

# manual calculations
s_residual = summary(m)$sigma
n =dim(t9e1)[1]

se_individual = s_residual * sqrt( 1 + 1/n + (175- mean(t9e1$Father))^2/(var(t9e1$Father)*(n-1)) )
t_cv = qt(0.95, n-2)

c(175.178 - t_cv*se_individual, 175.178 + t_cv*se_individual)


# --- 90% confidence interval for the mean response (population-level uncertainty) ---
predict(m, newdata, level = 0.90, interval = "confidence")


se_popu = s_residual * sqrt( 1/n + (175- mean(t9e1$Father))^2/(var(t9e1$Father)*(n-1)) )
c(175.178 - t_cv*se_popu, 175.178 + t_cv*se_popu)









