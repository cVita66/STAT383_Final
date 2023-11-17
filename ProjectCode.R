################################################################################
#
#   Project: Linear Regression Analysis of ___________
#   Author: Vincent Cifone
#   Date: 11/14/23
#   DataSet: WineQuality_Red
#   
#   STAT383 Statistics and Probability Final Project
#
#
################################################################################

# Access to ggplot data visualization
##install.packages("ggplot2") 
library(ggplot2)
setwd("C:/Users/vcifo/Documents/STAT383_FInal")

# Clean Console and Environment
rm(list = ls())
cat("\f")

#Access Dataset's
library(readxl)
WineProperties_Red <- read_excel("RedWine_DataSet.xlsx")
WineProperties_White <- read_excel("WhiteWine_DataSet.xlsx")

# Summary of Wine Quality Dataset (Red)
summary(WineProperties_Red)

# Scatter Plot: Alcohol Vs. pH
ggplot(WineProperties_Red, aes(x=pH, y=alcohol)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color="blue") +
  facet_wrap(~quality) +
  theme_minimal() +
  labs(title="Red Wine: Alcohol Vs. pH", x="Alcohol",y="pH")

# Summary of Wine Quality Dataset (White)
summary(WineProperties_White)

# Scatter Plot: Alcohol Vs. pH
ggplot(WineProperties_White, aes(x=pH, y=alcohol)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color="blue") +
  facet_wrap(~quality) +
  theme_minimal() +
  labs(title="White Wine: Alcohol Vs. pH", x="Alcohol",y="pH")



