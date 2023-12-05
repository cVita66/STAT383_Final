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

# New comlumn to identify high quality wines
redWineRandom$HighQuality <- as.factor(ifelse(redWineRandom$quality > median(redWineRandom$quality), 1, 0))

highQualityWine <- redWineRandom[redWineRandom$HighQuality != 0,]
lowQualityWine <- redWineRandom[redWineRandom$HighQuality == 0,]

# Creation of linear models -- Quality w/ Alcohol  from High Quality Wines
hq_alc_lr <- lm(quality ~ alcohol, data = highQualityWine)
print(summary(hq_alc_lr))

# Creation of linear models -- Quality w/ Alcohol  from Low Quality Wines
lq_alc_lr <- lm(quality ~ alcohol , data = lowQualityWine)
print(summary(lq_alc_lr))

# Creation of linear models -- Quality w/ Sugar  from High Quality Wines
hq_sug_lr <- lm(quality ~ `residual sugar`, data = highQualityWine)
print(summary(hq_sug_lr))

# Creation of linear models -- Quality w/ Sugar  from Low Quality Wines
lq_sug_lr <- lm(quality ~ `residual sugar` , data = lowQualityWine)
print(summary(lq_sug_lr))

# Creation of linear models -- Quality w/ pH  from High Quality Wines
hq_pH_lr <- lm(quality ~ pH, data = highQualityWine)
print(summary(hq_pH_lr))

# Creation of linear models -- Quality w/ pH  from Low Quality Wines
lq_pH_lr <- lm(quality ~ pH , data = lowQualityWine)
print(summary(lq_pH_lr))

# Creation of linear models -- Quality w/ Alcohol, pH  from High Quality Wines
hq_alc_pH_mlr <- lm(quality ~ alcohol + pH , data = highQualityWine)
print(summary(hq_alc_pH_mlr))

# Creation of linear models -- Quality w/ Alcohol, pH  from Low Quality Wines
lq_alc_pH_mlr <- lm(quality ~ alcohol + pH , data = lowQualityWine)
print(summary(lq_alc_pH_mlr))

# Creation of linear models -- Quality w/ Alcohol, pH and Sugar from High Quality Wines
hq_alc_pH_sugar_mlr <- lm(quality ~ alcohol + pH + `residual sugar`, data = highQualityWine)
print(summary(hq_alc_pH_sugar_mlr))

# Creation of linear models -- Quality w/ Alcohol, pH and Sugar from low Quality Wines
lq_alc_pH_sugar_mlr <- lm(quality ~ alcohol + pH + `residual sugar`, data = lowQualityWine)
print(summary(lq_alc_pH_sugar_mlr))




# Scatter Plot: Alcohol Vs. Sugar in High Quality Wines
ggplot(highQualityWine, aes(x=`residual sugar` , y=alcohol)) +
  geom_point(aes(size=pH)) +
  geom_smooth(method='lm', se=FALSE, color="blue") +
  theme_minimal() +
  labs(title="Red Wine: Alcohol vs. Sugar", x="Sugar",y="Alcohol")


# Scatter Plot: Alcohol Vs. pH~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggplot(highQualityWine, aes(x=pH , y=alcohol, color = `residual sugar`)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color="blue") +
  theme_minimal() +
  labs(title="Red Wine: Alcohol vs. pH", x="pH",y="Alcohol")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Scatter Plot: Alcohol Vs. Sugar in High Quality Wines
ggplot(redWineRandom, aes(x=`residual sugar` , y=alcohol, col=HighQuality)) +
  geom_point(aes(size=pH)) +
  geom_smooth(method='lm', se=FALSE, color="blue") +
  theme_minimal() +
  labs(title="Red Wine: Alcohol vs. Sugar", x="Sugar",y="Alcohol")


# Scatter Plot: Alcohol Vs. Sugar
ggplot(redWineRandom, aes(x=`residual sugar` , y=pH)) +
  geom_point(aes(size=HighQuality)) +
  geom_smooth(method='lm', se=FALSE, color="blue") +
  facet_wrap(~HighQuality) +
  theme_minimal() +
  labs(title="Red Wine: Sugar vs. pH", x="Sugar",y="pH")








