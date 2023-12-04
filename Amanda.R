#Amanda Polarolo
# Vincent Cifone
##########################
###Creation of a Multi-linear model for Alcohol, pH and Sulphites with respect to Quality

##setwd("C:/Users/Amanda/Documents/GitHub/STAT383_Final")  
# Access to ggplot data visualization
install.packages("ggplot2")         
library(ggplot2)

#Access Dataset's
install.packages("readxl")         
library(readxl)

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
#####

# Creation of linear models -- Quality VS alcohol~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
red_alc_pH_sulphates_mlr <- lm(quality ~ alcohol + pH + `sulphates`, data = redWineRandom)
print(summary(red_alc_pH_sulphates_mlr))

# Scatter Plot: Alcohol Vs. Sulphates
ggplot(redWineRandom, aes(x=`sulphates` , y=alcohol, col=quality)) +
  geom_point(aes(size=pH)) +
  geom_smooth(method='lm', se=FALSE, color="red") +
  theme_minimal() +
  labs(title="Red Wine: Alcohol vs. Sulphates", x="Sulphates",y="Alcohol")

# Scatter Plot: Alcohol Vs. Sulphates
ggplot(redWineRandom, aes(x=quality , y=pH, col=alcohol)) +
  geom_point(aes(size=`sulphates`)) +
  theme_minimal() +
  labs(title="Red Wine: Quality vs. pH", x="sulphates",y="Alcohol")

# Scatter Plot: Alcohol Vs. Sulphates
ggplot(redWineRandom, aes(x=`sulphates` , y=alcohol, col=pH)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color="red") +
  facet_wrap(~quality) +
  theme_minimal() +
  labs(title="Red Wine: Alcohol vs. Sulphates", x="Sulphates",y="Alcohol")


