#Amanda Polarolo
# Vincent Cifone
##########################
###Creation of a Multi-linear model for Alcohol, pH and Sulphites with respect to Quality

setwd("C:/Users/Amanda/Documents/GitHub/STAT383_Final")  
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


#------------------------------------------------------------------------------------------------#
#Total Sulfur Dioxide#

# Creation of linear models -- Quality VS alcohol~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
red_alc_pH_sulphites_mlr <- lm(quality ~ alcohol + pH + `total sulfur dioxide`, data = redWineRandom)
print(summary(red_alc_pH_sulphites_mlr))

# Scatter Plot: Alcohol Vs. Sulphites
ggplot(redWineRandom, aes(x=`total sulfur dioxide` , y=alcohol, col=quality)) +
  geom_point(aes(size=pH)) +
  geom_smooth(method='lm', se=FALSE, color="red") +
  theme_minimal() +
  labs(title="Red Wine: Alcohol vs. Sulphites", x="Sulphites",y="Alcohol")

# Scatter Plot: Alcohol Vs. Sulphites
ggplot(redWineRandom, aes(x=quality , y=pH, col=alcohol)) +
  geom_point(aes(size=`total sulfur dioxide`)) +
  theme_minimal() +
  labs(title="Red Wine: Quality vs. pH", x="Sulphites",y="Alcohol")

# Scatter Plot: Alcohol Vs. Sulphites
ggplot(redWineRandom, aes(x=`total sulfur dioxide` , y=alcohol, col=pH)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color="red") +
  facet_wrap(~quality) +
  theme_minimal() +
  labs(title="Red Wine: Alcohol vs. Sulphites", x="Sulphites",y="Alcohol")

#------------------------------------------------------------------------------------------------#
#Free Sulfur Dioxide#

# Creation of linear models -- Quality VS alcohol~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
red_alc_pH_sulphites_mlr <- lm(quality ~ alcohol + pH + `free sulfur dioxide`, data = redWineRandom)
print(summary(red_alc_pH_sulphites_mlr))

# Scatter Plot: Alcohol Vs. Free Sulphites
ggplot(redWineRandom, aes(x=`free sulfur dioxide` , y=alcohol, col=quality)) +
  geom_point(aes(size=pH)) +
  geom_smooth(method='lm', se=FALSE, color="red") +
  theme_minimal() +
  labs(title="Red Wine: Alcohol vs. Free Sulphites", x="Sulphites",y="Alcohol")

# Scatter Plot: Alcohol Vs. Free Sulphites
ggplot(redWineRandom, aes(x=quality , y=pH, col=alcohol)) +
  geom_point(aes(size=`free sulfur dioxide`)) +
  theme_minimal() +
  labs(title="Red Wine: Quality vs. pH", x="Sulphites",y="Alcohol")

# Scatter Plot: Alcohol Vs. Free Sulphites
ggplot(redWineRandom, aes(x=`free sulfur dioxide` , y=alcohol, col=pH)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color="red") +
  facet_wrap(~quality) +
  theme_minimal() +
  labs(title="Red Wine: Alcohol vs. Sulphites", x="Sulphites",y="Alcohol")
