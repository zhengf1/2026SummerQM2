## this is a addition
1+1

## define a variable
a = 20

## add a value to the variable
a + 3

## this is a comment
# nothing would be executed

## this is a multiplication
a * 8

## set working directory 
# navigate to the folder that contains your data file
# click on blue gear and set as working directory
# or use syntax (your path can be different)
setwd("/Users/zhengfan/Dropbox/01 UoM-Teaching/2026-Summer-QM2/R_tute1")

## load the data
# under Environment, click on Import Dataset
# or use syntax
# install.packages("readxl")  
library(readxl) # you need to install before use it
zimomo <- read_excel("t1e2.xlsx") # name if whatever you like




