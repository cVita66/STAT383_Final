################################################################################
#
#   Project: Linear Regression Analysis of Wine Quality
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

##install.packages("flextable")       
library(flextable)

# Clean Console and Environment
rm(list = ls())
cat("\f")

# Local access to datasets
WineProperties_Red <- read_excel("RedWine_DataSet.xlsx")

##### Get Random Sample of 500 red wines
#Seed for random number generator using pseudo-random number generation which is sufficiently random for our purposes
set.seed(5678)

#Find indexes of the 500 random elements
randomIndexes <- sample(seq_len(nrow(WineProperties_Red)), 500, replace = FALSE)

#Get random data from random index (can choose which data to set after ",")
redWineRandom <- WineProperties_Red[randomIndexes, ]
################################################################################
## Cifone, Vincent Coding Contribution

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##-- Quality w/ pH and chlorides within respective ranges
q_ph_chlor_mlm <- lm(quality ~ pH + chlorides, data = redWineRandom)

# Scatter Plot: Quality Vs. Chlorides with feature pH by color
plot_quality_ch_pH <- ggplot(redWineRandom, aes(x=chlorides , y=quality, color=pH)) +
  geom_point( stroke=3, alpha = 0.5)+
  geom_smooth(method='lm', se=FALSE, color="blue") +
  theme_minimal() +
  labs(title = "Quality vs Chlorides and pH", x="Chloride Concentration",y="Quality")

## Display Graph
plot_quality_ch_pH

# Scatter Plot: Quality Vs. Chlorides
plot_quality_ch <-ggplot(redWineRandom, aes(x=chlorides , y=quality)) +
  geom_point( stroke=3, alpha = 0.5, color = "lightblue")+
  geom_smooth(method='lm', se=FALSE, color="blue") +
  theme_minimal() +
  labs(title = "Quality vs Chlorides", x="Chloride Concentration",y="Quality")

## Display Graph
plot_quality_ch

# Scatter Plot: Quality Vs. pH 
plot_quality_pH <-ggplot(redWineRandom, aes(x=pH , y=quality)) +
  geom_point(stroke=3, alpha = 0.5, color = "lightblue")+
  geom_smooth(method='lm', se=FALSE, color="blue") +
  theme_minimal() +
  labs(title = "Quality vs pH", x="pH Levels",y="Quality")

## Display Graph
plot_quality_pH

# Tables for the linear models
pH_Cl_table <- as_flextable(q_ph_chlor_mlm)
print(pH_Cl_table)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##-- Quality w/ pH and sulfites within respective ranges
q_ph_freeSulf_mlm <- lm(quality ~ pH + `free sulfur dioxide`, data = redWineRandom)

# Scatter Plot: Quality Vs. Sulfites with feature pH by color
plot_quality_sulf_pH <- ggplot(redWineRandom, aes(x=`free sulfur dioxide` , y=quality, color=pH)) +
  geom_point( stroke=3, alpha = 0.5)+
  geom_smooth(method='lm', se=FALSE, color="blue") +
  theme_minimal() +
  labs(title = "Quality vs Sulfites and pH", x="Sulfite Content",y="Quality")

## Display Graph
plot_quality_sulf_pH

# Tables for the linear models
pH_Sulf_table <- as_flextable(q_ph_freeSulf_mlm)
print(pH_Sulf_table)

# End Cifone
################################################################################
## Gasiewicz, Colin Coding Contribution

redWine <- subset(redWineRandom, chlorides < 0.15)

# Creation of linear models -- chlorides VS quality) 
QCl_lm <- lm(quality ~ chlorides + alcohol , data = redWine)

plot_quality_cl_alc <- ggplot(redWine, aes(x = chlorides, y = quality)) + 
  geom_point(aes(size = alcohol), alpha = 0.15, color = "lightblue") +
  geom_smooth(method = "lm", se = FALSE, linetype = "solid", color = "blue") +
  labs(title = "Quality vs Chlorides and Alcohol", x = "Chlorides Concentration", y = "Quality") +
  theme_minimal() 

## Display Graph
plot_quality_cl_alc

#Better table
table <- as_flextable(QCl_lm)
print(table)

# End Gasiewicz
################################################################################
## Polarolo, Amanda Coding Contribution


# Creation of linear models -- Quality VS sugar and chlorides
red_sug_chlor_quality_mlr <- lm(quality ~ `residual sugar` + chlorides, data = redWineRandom)

# Scatter Plot: Sugar , quality
plot_quality_sugar <- ggplot(redWineRandom, aes(x=`residual sugar` , y=quality)) +
  geom_point(stroke=3, alpha = 0.5, color = "lightblue") +
  geom_smooth(method='lm', se=FALSE, color="blue") +
  labs(title = "Quality vs Sugar", x = "Sugar Content", y = "Quality") +
  theme_minimal() 

## Display Graph
plot_quality_sugar

table <- as_flextable(red_sug_chlor_quality_mlr)
print(table)

# End Polarolo
################################################################################


