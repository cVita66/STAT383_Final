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

library(broom)
library(dplyr)
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

###### Creation of specialized datasets 

#New column of Wines within pH range
redWineRandom$InpHRange <- as.factor(ifelse(redWineRandom$pH > 3.2 & redWineRandom$pH < 3.4, 1,0 ))
#New column of Wines within Chloride range
redWineRandom$InChlRange <- as.factor(ifelse(redWineRandom$chlorides > 0.02 & redWineRandom$chlorides < 0.05, 1,0 ))

# With Limited Chloride Range
hQ_clLimit_Wine <- redWineRandom[redWineRandom$InChlRange != 0,]
# Within ph Range
hQ_clLimit_pHLimit_Wine <- hQ_clLimit_Wine[hQ_clLimit_Wine$InpHRange != 0,]

###### Creation of linear models and graphs
##-- Quality w/ pH and chlorides within respective ranges
q_ph_chlor_mlm <- lm(quality ~ pH + chlorides, data = redWineRandom)


# Scatter Plot: Alcohol Vs. Sugar in All Quality Wines
ggplot(redWineRandom, aes(x=pH , y=chlorides, color=quality)) +
  geom_point(fill="lightblue", stroke=1, alpha = 0.5)+
  geom_smooth(method='lm', se=FALSE, color="blue") +
  #facet_wrap(~quality) +
  theme_minimal() +
  labs( x="pH %",y="Chlorides %")


###### Creation of linear models and graphs
##-- Quality (High) w/ pH and chlorides within respective ranges
q_pHL_chlorL_mlm <- lm(quality ~ pH + chlorides, data = hQ_clLimit_pHLimit_Wine)


# Scatter Plot: Alcohol Vs. Sugar in High Quality Wines
ggplot(hQ_clLimit_pHLimit_Wine, aes(x=pH , y=chlorides, color=quality)) +
  geom_point(fill="lightblue", stroke=1, alpha = 0.5)+
  geom_smooth(method='lm', se=FALSE, color="blue") +
  #facet_wrap(~quality) +
  theme_minimal() +
  labs( x="pH %",y="Chlorides %")


# Summary of the linear models
print(summary(q_ph_chlor_mlm))
print(summary(q_pHL_chlorL_mlm))

# Tables for the linear models
no_limit_pH_Cl_table <- as_flextable(q_ph_chlor_mlm)
print(no_limit_pH_Cl_table)

limit_pH_Cl_table <- as_flextable(q_pHL_chlorL_mlm)
print(limit_pH_Cl_table)





















