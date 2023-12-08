################################################################################
#
#   Project: Linear Regression Analysis of ___________
#   Author: Vincent Cifone
#           Murdoch DeGray
#           Amanda Polarolo
#           Alexander Seeley
#           Colin Gasiewicz
#   Date: 11/14/23
#   DataSet: WineQuality_Red
#   
#   STAT383 Statistics and Probability Final Project
#
#
################################################################################
##setwd("C:/Users/___/Documents/STAT383_Final")  <-- Unqiue per user
# Access to ggplot data visualization
##install.packages("ggplot2")         // If ggplot2 not installed
library(ggplot2)

#Access Dataset's
##install.packages("readxl")          // If readxl not installed
library(readxl)

# Clean Console and Environment
rm(list = ls())
cat("\f")

# Local access to datasets
WineProperties_Red <- read_excel("RedWine_DataSet.xlsx")