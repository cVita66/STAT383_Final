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

##df$high_mpg <- as.factor(ifelse(df$mpg > median(df$mpg), 1, 0))
WineProperties_Red$HighQuality <- as.factor(ifelse(WineProperties_Red$quality > median(WineProperties_Red$quality), 1, 0))

# Response and explanatory variables
# given these, want to model the linear model find the most significant predictors(explanatory vairables, p values < 0.05)
# multiple linear regression --> rather not see one output one input, multiple inputs with one output
#first paper outlines stepwise linear regression, example of code for linear regression (different for logistic)
# step could use logistic, same could, use the quality as the output


# Creation of linear models -- Quality VS pH and Residual Sugar~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
red_fit_pHnSug_lm <- lm(quality ~ pH + `residual sugar`, data = WineProperties_Red)
print(summary(red_fit_pHnSug_lm))

wht_fit_pHnSuglm <- lm(quality ~ pH + `residual sugar`, data = WineProperties_White)
print(summary(wht_fit_pHnSuglm))

# Creation of linear models -- Quality VS pH and alcohol~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
red_fit_pHnAlclm <- lm(quality ~ pH + alcohol, data = WineProperties_Red)
print(summary(red_fit_pHnAlclm))

wht_fit_pHnAlclm <- lm(quality ~ pH + alcohol, data = WineProperties_White)
print(summary(wht_fit_pHnAlclm))

# Creation of linear models -- Quality VS pH ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
red_fit_pHlm <- lm(quality ~ pH , data = WineProperties_Red)
print(summary(red_fit_pHlm))

wht_fit_pHlm <- lm(quality ~ pH , data = WineProperties_White)
print(summary(wht_fit_pHlm))

# Creation of linear models -- Quality VS citric acid, density ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
red_fit_CDlm <- lm(quality ~ `citric acid` + density , data = WineProperties_Red)
print(summary(red_fit_CDlm))

wht_fit_CDlm <- lm(quality ~ `citric acid` + density , data = WineProperties_White)
print(summary(wht_fit_CDlm))

# Creation of linear models -- Quality VS Residual Sugar~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
red_fit_Sug_lm <- lm(quality ~  `residual sugar`, data = WineProperties_Red)
print(summary(red_fit_Sug_lm))

wht_fit_Suglm <- lm(quality ~  `residual sugar`, data = WineProperties_White)
print(summary(wht_fit_Suglm))

# Creation of linear models -- Quality VS alcohol~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
red_fit_Alclm <- lm(quality ~ alcohol, data = WineProperties_Red)
print(summary(red_fit_Alclm))

wht_fit_Alclm <- lm(quality ~ alcohol, data = WineProperties_White)
print(summary(wht_fit_Alclm))

# Summary of Wine Quality Dataset (Red)
##summary(WineProperties_Red)

# Scatter Plot: Alcohol Vs. Sugar
ggplot(WineProperties_Red, aes(x=`residual sugar`, y=alcohol)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color="blue") +
  facet_wrap(~quality) +
  theme_minimal() +
  labs(title="Red Wine: Alcohol Vs. Sugar", x="Alcohol",y="Sugar")

# Summary of Wine Quality Dataset (White)
##summary(WineProperties_White)

# Scatter Plot: Alcohol Vs. Sugar
ggplot(WineProperties_White, aes(x=`residual sugar`, y=alcohol)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color="blue") +
  facet_wrap(~quality) +
  theme_minimal() +
  labs(title="White Wine: Alcohol Vs. Sugar", x="Alcohol",y="Sugar")



