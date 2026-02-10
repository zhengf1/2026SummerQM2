# Install Packages (if not already installed)
# install.packages(c("sandwich","pastecs", "Hmisc", "car", "lmtest"))

# Read in data either via File->Import Dataset and follow instruction concerning wide and long format or using code below (you will need to end the relevant disk address)
# Reads data from an Excel file named "t10e4.xlsx" located at "C:/.../t10e4.xlsx"

setwd("~/Dropbox/01 UoM-Teaching/2026-S0-Summer-QM2/Tute11")
setwd("C:/Users/zhefan/Downloads/Tute11/Tute11")

library(readxl)
t10e4 <- read_excel("t10e4.xlsx")

# Fit logistic regression model
logit = glm(COKE ~ PRATIO + DISP_COKE + DISP_PEPSI,   # Model formula: COKE as response variable, PRATIO, DISP_COKE, and DISP_PEPSI as predictor variables
            family = binomial(link = "logit"),       # Binomial family with logit link function
            data = t10e4)                            # Data from t10e4 dataframe
summary(logit)

# deviance smaller the better. 
# smallest deviance is 0, and the corresponding
# model is called saturated model

# for your own interest, null_deviance can be directly
# calculate using the formula dev = -2 * (n_s*log(p)+n_f*log(1-p))
p_hat = mean(t10e4$COKE)
num_succ = sum(t10e4$COKE==1)
num_fail = sum(t10e4$COKE==0)
null_dev = -2 * (num_succ*log(p_hat) + num_fail*log(1-p_hat))

# McFadden's Pseudo R-squared based on deviance
-((1418.9 - 1567.7) / 1567.7)

# Load DescTools package for calculating Pseudo R-squared
library(DescTools) 
PseudoR2(logit, which = "McFadden")                  # Calculate McFadden's Pseudo R-squared

# Load lmtest package for likelihood ratio test
library(lmtest) 
lrtest(logit)                                         # Perform likelihood ratio test

# Calculate predicted values of COKE using the fitted logistic regression model
COKE_logit = round(fitted.values(logit), digits = 0) 
# Calculate proportions of observed COKE values compared to predicted COKE values
prop.table(table(t10e4$COKE, COKE_logit))

# Create new data frame for prediction
newdata = data.frame(PRATIO = mean(t10e4$PRATIO),     # PRATIO value is set to the mean of PRATIO in t10e4
                     DISP_COKE = c(1, 0),             # Two scenarios for DISP_COKE: 1 (present) and 0 (absent)
                     DISP_PEPSI = c(0, 1))            # Two scenarios for DISP_PEPSI: 0 (absent) and 1 (present)
# Predict log-odds of COKE for new data
predict.glm(logit, newdata, type = "link") 
(z = 1.9230 + (-1.9957)*1.027249+1*0.3516)

# Predict probabilities of COKE for new data
predict.glm(logit, newdata, type = "response")
(p = 1/(1+exp(-z)))

# Install and load margins package for calculating marginal effects
# install.packages("margins")
library(margins)
marginal_effects(logit, data = newdata)               # Calculate marginal effects of predictors on COKE probability






