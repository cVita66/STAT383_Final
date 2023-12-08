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

library(flextable)
library(vctrs)
library(broom)
##install.packages("flextable")
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
library(ggplot2)


redWine <- subset(redWineRandom, chlorides < 0.15)


# Creation of linear models -- chlorides VS quality) 
QCl_lm <- lm(quality ~ chlorides + alcohol , data = redWine)

print(summary(QCl_lm))

ggplot(redWine, aes(x = chlorides, y = quality)) + 
  geom_point(aes(size = alcohol), alpha = 0.15, color = "lightblue") +
  geom_smooth(method = "lm", se = FALSE, linetype = "solid", color = "blue") +
  labs(title = "Chlorides vs Quality vs. Alcohol", x = "Chlorides", y = "Quality") +
  theme_minimal() 
grid()

#Better table
##install.packages("flextable")
table <- as_flextable(QCl_lm)
print(table)


##########################
##End Colin.R

