# setwd("D:/CEU/winter semester/data-science-1")

install.packages('caret')
install.packages('party')
library(tidyverse)
require(scales)
library(skimr)
# library(plyr)



data <- readRDS(url('http://www.jaredlander.com/data/manhattan_Train.rds')) %>%
  mutate(logTotalValue = log(TotalValue)) %>%
  drop_na()

# EDA -------------------------------------------------------------------------
# There are 47 variables: 

# glimpse(data)
# str(data)

# 17 factor variables, 29 numeric variables, 1 logical
skim(data)

# 6K Extension, 25K unknown
table(data$Extension)

# Total Value skewed to the right
ggplot(data = data, aes(x = TotalValue)) +
  geom_histogram() + 
  labs(x = "Total Value") + 
  scale_x_continuous(labels = comma)

summary(data$TotalValue)
# Total Value varies between 1080 to 19991700

# closer to normal distribution
ggplot(data = data, aes(x = logTotalValue)) +
  geom_histogram() + 
  labs(x = "Total Value") + 
  scale_x_continuous(labels = comma)

ggplot(data = data, aes(x = BuiltFAR)) +
  geom_histogram() + 
  labs(x = "BuiltFAR") + 
  scale_x_continuous(labels = comma)


summary(data$BuiltFAR)

# ggpairs(data, columns = c("TotalValue", "BuiltFAR", "NumFloors", "ResArea", "BldgArea"), upper = list(continuous = wrap("cor", size = 4)))

# data[, .(TotalValue = mean(TotalValue)), by=.(LandUse)]


??skim
reg1_lm <- lm(TotalValue ~ LotArea, 
               data = subset(data,complete.cases(data)))
summary(reg1_lm)

ggplot(data, aes(log(LotArea), logTotalValue)) +
  geom_point()
??predict

reg2_lm <- lm(TotalValue ~ BldgArea, 
              data = subset(data, complete.cases(data)))
summary(reg2_lm)

ggplot(data, aes(log(BldgArea), logTotalValue)) +
  geom_point()

data$area



# Checking some scatter-plots:
# Create a general function to check the pattern
chck_sp <- function(x_var, x_lab){
  ggplot( data , aes(x = x_var, y = logTotalValue)) +
    geom_point() +
    geom_smooth(method="loess" , formula = y ~ x )+
    labs(y = "Log TotalValue of houses", x = x_lab) 
}

chck_sp(data$LotArea, "Lot Area")


ggplot( data , aes(x = log(LotArea), y = logTotalValue)) +
  geom_point() +
  geom_smooth(method="loess" , formula = y ~ x )+
  labs(y = "Log TotalValue of houses", x = "Lot Area") 

ggplot( data , aes(x = log(BldgArea), y = logTotalValue)) +
  geom_point() +
  geom_smooth(method="loess" , formula = y ~ x )+
  labs(y = "Log TotalValue of houses", x = "Lot Area") 

# possible predictors of the target variable: LotArea, BuildingArea

