# ===============================
# Load and View Data
# ===============================

# Read in data
# Either use File -> Import Dataset as instructed in previous tutorials and follow 
# instructions about wide vs. long formats, or read the files using the code below.
# You must update the file paths to match your computer.

library(readxl)  # For reading Excel files

# Change the path to your folder
setwd("~/Dropbox/01 UoM-Teaching/2026-Summer-QM2/Tute6")
# Load t6e1 dataset: Wide and Long format (important for different types of analyses)
t6e1_wide <- read_excel("t6e1.xlsx", sheet = "Wide")
View(t6e1_wide)

t6e1_long <- read_excel("t6e1.xlsx", sheet = "Long")
View(t6e1_long)

# Load Exercise 2 dataset
t6e2 <- read_excel("t6e2.xlsx")
View(t6e2)

# Load Exercise 3 dataset
t6e3 <- read_excel("t6e3.xlsx")
View(t6e3)

# ===============================
# Exercise 1: One-way ANOVA and assumptions
# ===============================

# (a) manual calculations
x_j_bar = sapply(t6e1_wide, mean) 
x_bar = mean(x_j_bar)

n1 = dim(t6e1_wide)[1] # = n2 = n3 = n4
k = dim(t6e1_wide)[2]

n = k*n1 

SST = n1*sum( (x_j_bar - x_bar)^2 )
x_j_sd = sapply(t6e1_wide, sd) 
SSE = sum( (n1-1)*x_j_sd^2 )

MST = SST/(k-1)
MSE = SSE/(n-k)

(F_statis = MST/MSE)

qf(0.05, k-1, n-k, lower.tail = FALSE)

library(pastecs)
# Summary stats and normality tests for wide-format data
round(stat.desc(t6e1_wide, basic = FALSE, desc = TRUE, norm = TRUE, p = 0.95), 3)

# One-way ANOVA using long-format data
summary(aov(t6e1_long$Time ~ t6e1_long$Form))
# or 
summary(aov(Time ~ Form,data = t6e1_long))

# if unfortunately, we don't have t6e1_long format,
# we need to construct the long format ourselves.

# Alternative approach: reshape data manually for analysis
minutes = c(t6e1_wide$Form1, t6e1_wide$Form2, t6e1_wide$Form3, t6e1_wide$Form4)
forms = gl(4, 30, 120, c("Form1", "Form2", "Form3", "Form4"))  # Create group labels
summary(aov(minutes ~ forms))  # Run ANOVA

# --- Assumption Checks ---

library(car)

# Levene's test for homogeneity of variances
leveneTest(minutes ~ forms)

# One-way test assuming equal variances
oneway.test(minutes ~ forms, var.equal = TRUE)
# The same as aov

# If we fail the Levene test, namely variance are unequal
# Welch's ANOVA (does not assume equal variances)
oneway.test(minutes ~ forms)


# ===============================
# Exercise 2: Nonparametric test (Kruskal-Wallis)
# ===============================

library(psych)

# Histograms to visually inspect the response distribution by Ad group
par(mfrow = c(2, 2))  # Set plotting area for 4 histograms
for (i in c(1, 2, 3, 4)) {
  hist(subset(t6e2$Response, t6e2$Ad == i),
       main = paste("Ad", i), xlab = "Response")
}
par(mfrow = c(1, 1))  # Remember execute this to set it back

# Kruskal-Wallis test for differences in medians across ad groups
kruskal.test(t6e2$Response, t6e2$Ad)


# ===============================
# Exercise 3: Comparing fertilizer types
# ===============================

# Combine columns into single response and group vectors
Weight = c(t6e3$None, t6e3$Biological, t6e3$Chemical)
Fertilizer = gl(3, 30, 90, c("None", "Biological", "Chemical"))

# One-way ANOVA
summary(aov(Weight ~ Fertilizer))

# Levene's test for equal variances
library(car)
leveneTest(Weight ~ Fertilizer)

# One-way test (Welch and standard)
oneway.test(Weight ~ Fertilizer)  # Welch ANOVA

# Nonparametric test for group medians
kruskal.test(Weight, Fertilizer)
