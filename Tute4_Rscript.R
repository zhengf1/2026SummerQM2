# ===============================
# Load and View Data
# ===============================

# Read in data
# Either use File -> Import Dataset as instructed in previous tutorials or
# read the files directly by modifying the paths below

library(readxl)  # Load the readxl package for reading Excel files

# Load t4e1 dataset
t4e1 <- read_excel("t4e1.xlsx")
View(t4e1)  # View t4e1 in the RStudio data viewer

# Load t4e2 dataset
t4e2 <- read_excel("t4e2.xlsx")
View(t4e2)  # View t4e2 in the RStudio data viewer


# ===============================
# Exercise 1
# ===============================

# --- Paired t-test ---
t4e1$D = t4e1$Pattern1 - t4e1$Pattern2
t.test(t4e1$D)
t.test(t4e1$Pattern1, t4e1$Pattern2, paired = TRUE)

# mean(t4e1$D)/(sd(t4e1$D)/sqrt(length(t4e1$D)))

# --- Sign Test ---
library(DescTools)
SignTest(t4e1$D)
SignTest(t4e1$Pattern1, t4e1$Pattern2)

# --- Descriptive statistics and normality checks ---
hist(t4e1$D, freq = FALSE, col = "yellow")
lines(seq(-0.1, 0.6, by = 0.01),
      dnorm(seq(-0.1, 0.6, by = 0.01), mean(t4e1$D), sd(t4e1$D)),
      col = "red")

qqnorm(t4e1$D, main = "Normal Q-Q Plot",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       col = "forestgreen")
qqline(t4e1$D, col = "blue")

library(pastecs)
round(stat.desc(t4e1$D, basic = FALSE, desc = TRUE, norm = TRUE), 3)

# --- Wilcoxon Signed-Rank Test ---
library(exactRankTests)
wilcox.exact(t4e1$D)
wilcox.exact(t4e1$Pattern1, t4e1$Pattern2, paired = TRUE)


# ===============================
# Exercise 2
# ===============================

# --- Group-wise summary statistics ---
by(t4e2$Age, t4e2$Householder, mean)
by(t4e2$Age, t4e2$Householder, sd)

library(pastecs)
stat.desc(subset(t4e2$Age, t4e2$Householder == "N"),
          basic = FALSE, desc = TRUE, norm = TRUE)
stat.desc(subset(t4e2$Age, t4e2$Householder == "P"),
          basic = FALSE, desc = TRUE, norm = TRUE)

# --- Independent Samples t-tests ---
t.test(t4e2$Age ~ t4e2$Householder, conf.level = 0.90)
t.test(t4e2$Age ~ t4e2$Householder, var.equal = TRUE, conf.level = 0.90)

P = t4e2$Age[t4e2$Householder=="P"]
N = t4e2$Age[t4e2$Householder=="N"]

#D = mean(N) - mean(P) 
#se_square = sd(N)^2/length(N) + sd(P)^2/length(P)  
#D/sqrt(se_square)


# --- Wilcoxon Rank-Sum Test ---
library(exactRankTests)
wilcox.exact(t4e2$Age ~ t4e2$Householder)
wilcox.exact(t4e2$Age ~ t4e2$Householder, alternative = "less")
wilcox.exact(t4e2$Age ~ t4e2$Householder, alternative = "greater")

