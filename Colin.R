#Colin.R
##########################
# Generation of 500 wines form RedWine_Dataset.xlxs
## Requires
### "RedWine_Dataset.xlxs" must exist in the current working directory
## Returns 
### redWineRandom: a sample of 500 random points of data
### randomIndexes: the corresponding random indexes form WineProperties_Red 
## Dependencies
### install.packages("readxl")          // If readxl not installed
library(readxl)

# Clean Console and Environment
rm(list = ls())
cat("\f")

#importing red wine data from excel file
WineProperties_Red <- read_excel("RedWine_DataSet.xlsx")

#Seed for random number generator using pseudo-random number generation which is sufficiently random for our purposes
set.seed(5678)

#Find indexes of the 500 random elements
randomIndexes <- sample(seq_len(nrow(WineProperties_Red)), 500, replace = FALSE)

#Get random data from random index (can choose which data to set after ",")
redWineRandom <- WineProperties_Red[randomIndexes, ]

# Chlorides and Quality linear models
## Requires
### "redWineRandom" must exist
## Returns 
###
## Dependencies
### install.packages("ggplot2")         // If ggplot2 not installed
library(ggplot2)

# Creation of linear models -- chlorides VS quality) 
QCl_lm <- lm(chlorides ~ quality , data = redWineRandom)
print(summary(QCl_lm))

# Scatter Plot: Chlorides Vs. Quality
ggplot(WineProperties_Red, aes(x=`chlorides`, y=quality)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color="blue") +
  facet_wrap(~quality) +
  theme_minimal() +
  labs(title="Red Wine: chlorides Vs. quality", x="Chlorides",y="Quality")







##########################
##End Colin.R

