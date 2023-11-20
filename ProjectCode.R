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
WineProperties_White <- read_excel("WhiteWine_DataSet.xlsx")

# Creation of linear models -- Quality VS pH and Residual Sugar
red_fit_pHnSug_lm <- lm(quality ~ pH + `residual sugar`, data = WineProperties_Red)
print(summary(red_fit_pHnSug_lm))

wht_fit_pHnSuglm <- lm(quality ~ pH + `residual sugar`, data = WineProperties_White)
print(summary(wht_fit_pHnSuglm))

# Creation of linear models -- Quality VS pH and alcohol
red_fit_pHnAlclm <- lm(quality ~ pH + alcohol, data = WineProperties_Red)
print(summary(red_fit_pHnAlclm))

# Creation of linear models
wht_fit_pHnAlclm <- lm(quality ~ pH + alcohol, data = WineProperties_White)
print(summary(wht_fit_pHnAlclm))


# Creation of linear models -- Quality VS alcohol
red_fit_Alclm <- lm(quality ~ alcohol, data = WineProperties_Red)
print(summary(red_fit_Alclm))

# Creation of linear models
wht_fit_Alclm <- lm(quality ~ alcohol, data = WineProperties_White)
print(summary(wht_fit_Alclm))

# Creation of linear models -- Quality VS pH 
red_fit_pHlm <- lm(quality ~ pH , data = WineProperties_Red)
print(summary(red_fit_pHlm))

# Creation of linear models
wht_fit_pHlm <- lm(quality ~ pH , data = WineProperties_White)
print(summary(wht_fit_pHlm))

# Creation of linear models -- Quality VS citric acid, density 
red_fit_CDlm <- lm(quality ~ `citric acid` + density , data = WineProperties_Red)
print(summary(red_fit_CDlm))

# Creation of linear models
wht_fit_CDlm <- lm(quality ~ `citric acid` + density , data = WineProperties_White)
print(summary(wht_fit_CDlm))

# Creation of linear models -- Quality VS Residual Sugar
red_fit_Sug_lm <- lm(quality ~  `residual sugar`, data = WineProperties_Red)
print(summary(red_fit_Sug_lm))

wht_fit_Suglm <- lm(quality ~  `residual sugar`, data = WineProperties_White)
print(summary(wht_fit_Suglm))

# Summary of Wine Quality Dataset (Red)
##summary(WineProperties_Red)

# Scatter Plot: Alcohol Vs. pH
##ggplot(WineProperties_Red, aes(x=pH, y=alcohol)) +
##  geom_point() +
##  geom_smooth(method='lm', se=FALSE, color="blue") +
##  facet_wrap(~quality) +
##  theme_minimal() +
##  labs(title="Red Wine: Alcohol Vs. pH", x="Alcohol",y="pH")

# Summary of Wine Quality Dataset (White)
##summary(WineProperties_White)

# Scatter Plot: Alcohol Vs. pH
##ggplot(WineProperties_White, aes(x=pH, y=alcohol)) +
##  geom_point() +
##  geom_smooth(method='lm', se=FALSE, color="blue") +
##  facet_wrap(~quality) +
##  theme_minimal() +
##  labs(title="White Wine: Alcohol Vs. pH", x="Alcohol",y="pH")



