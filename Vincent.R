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
randomIndexes <- sample(seq_len(nrow(WineProperties_Red)), 2000, replace = FALSE)

#Get random data from random index (can choose which data to set after ",")
redWineRandom <- WineProperties_Red[randomIndexes, ]

###### Creation of specialized datasets 

# New comlumn to identify high quality wines
redWineRandom$HighQuality <- as.factor(ifelse(redWineRandom$quality > median(redWineRandom$quality), 1, 0))
#New column of Wines within pH range
redWineRandom$InpHRange <- as.factor(ifelse(redWineRandom$pH > 3.2 & redWineRandom$pH < 3.4, 1,0 ))
#New column of Wines within Chloride range
redWineRandom$InChlRange <- as.factor(ifelse(redWineRandom$chlorides > 0.02 & redWineRandom$chlorides < 0.05, 1,0 ))

# High Quality
hQ_Wine <- redWineRandom[redWineRandom$HighQuality != 0,]
# Within Chloride Range
hQ_cl_Wine <- hQ_Wine[hQ_Wine$InChlRange != 0,]
# Within ph Range
hQ_cl_pH_Wine <- hQ_cl_Wine[hQ_cl_Wine$InpHRange != 0,]

###### Creation of linear models and graphs
##-- Quality (High) w/ pH and chlorides within respective ranges
hq_cl_pH_lm <- lm(quality ~ pH + chlorides, data = hQ_cl_pH_Wine)
print(summary(hq_cl_pH_lm))


# Scatter Plot: Alcohol Vs. Sugar in High Quality Wines
ggplot(hQ_cl_pH_Wine, aes(x=pH , y=chlorides)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color="blue") +
  facet_wrap(~quality) +
  theme_minimal() +
  labs(title="Red Wine: Chlorides vs. pH Per Quality", x="pH",y="Chlorides")



###### Creation of linear models and graphs
##-- Quality (High) w/ pH and chlorides within respective ranges
hq_lm <- lm(quality ~ pH + chlorides, data = hQ_Wine)
print(summary(hq_lm))


# Scatter Plot: Alcohol Vs. Sugar in High Quality Wines
ggplot(hQ_Wine, aes(x=pH , y=chlorides)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color="blue") +
  facet_wrap(~quality) +
  theme_minimal() +
  labs(title="Red Wine: Chlorides vs. pH Per Quality", x="pH",y="Chlorides")



###### Creation of linear models and graphs
##-- Quality (High) w/ pH and chlorides within respective ranges
hq_lm <- lm(quality ~ pH + chlorides, data = redWineRandom)
print(summary(hq_lm))


# Scatter Plot: Alcohol Vs. Sugar in High Quality Wines
ggplot(redWineRandom, aes(x=pH , y=chlorides)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color="blue") +
  facet_wrap(~quality) +
  theme_minimal() +
  labs(title="Red Wine: Chlorides vs. pH Per Quality", x="pH",y="Chlorides")


















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








