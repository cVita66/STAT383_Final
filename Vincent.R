# Vincent Cifone
##########################
###Creation of a Multi-linear model for Alcohol, pH and Sugar with respect to Quality
#@Requires
#"RedWine_Dataset.xlxs" must exist in the current working directory
#
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

##### Get Random Sample of 500 red wines
#Seed for random number generator using pseudo-random number generation which is sufficiently random for our purposes
set.seed(5678)

#Find indexes of the 500 random elements
randomIndexes <- sample(seq_len(nrow(WineProperties_Red)), 200, replace = FALSE)

#Get random data from random index (can choose which data to set after ",")
redWineRandom <- WineProperties_Red[randomIndexes, ]
#####

# Creation of linear models -- Quality VS alcohol~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
red_alc_pH_sugar_mlr <- lm(quality ~ alcohol + pH + `residual sugar`, data = redWineRandom)
print(summary(red_alc_pH_sugar_mlr))

# Scatter Plot: Alcohol Vs. Sugar
ggplot(redWineRandom, aes(x=`residual sugar` , y=alcohol, col=quality)) +
  geom_point(aes(size=pH)) +
  geom_smooth(method='lm', se=FALSE, color="blue") +
  theme_minimal() +
  labs(title="Red Wine: Alcohol vs. Sugar", x="Sugar",y="Alcohol")

# Scatter Plot: Alcohol Vs. Sugar
ggplot(redWineRandom, aes(x=quality , y=pH, col=alcohol)) +
  geom_point(aes(size=`residual sugar`)) +
  theme_minimal() +
  labs(title="Red Wine: Quality vs. pH", x="Sugar",y="Alcohol")

# Scatter Plot: Alcohol Vs. Sugar
ggplot(redWineRandom, aes(x=`residual sugar` , y=alcohol, col=pH)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color="blue") +
  facet_wrap(~quality) +
  theme_minimal() +
  labs(title="Red Wine: Alcohol vs. Sugar", x="Sugar",y="Alcohol")











