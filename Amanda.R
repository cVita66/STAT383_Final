#Amanda Polarolo
# Vincent Cifone
##########################
###Creation of a Multi-linear model for Alcohol, pH and Sulphites with respect to Quality

#setwd("C:/Users/Amanda/Documents/GitHub/STAT383_Final")  
# Access to ggplot data visualization
install.packages("ggplot2")         
library(ggplot2)

#Access Dataset's
install.packages("readxl")         
library(readxl)
install.packages("flextable")       
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
randomIndexes <- sample(seq_len(nrow(WineProperties_Red)), 200, replace = FALSE)

#Get random data from random index (can choose which data to set after ",")
redWineRandom <- WineProperties_Red[randomIndexes, ]

######Sugar, Chlorides and Quality#

# Creation of linear models -- Quality VS sugar and chlorides~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
red_sug_chlor_quality_mlr <- lm(quality ~ `residual sugar` + chlorides, data = redWineRandom)
print(summary(red_sug_chlor_quality_mlr))

# Scatter Plot: Sugar, chlorides, quality
ggplot(redWineRandom, aes(x=`residual sugar` , y=quality)) +
  geom_point(stroke=3, alpha = 0.5, color = "lightblue") +
  geom_smooth(method='lm', se=FALSE, color="blue") +
  theme_minimal() 

# Scatter Plot: Sugar, chlorides, quality
ggplot(redWineRandom, aes(x=chlorides , y=quality)) +
  geom_point(stroke=3, alpha = 0.5, color = "lightblue") +
  geom_smooth(method='lm', se=FALSE, color="blue") +
  theme_minimal() 

 table <- as_flextable(red_sug_chlor_quality_mlr)
print(table)
