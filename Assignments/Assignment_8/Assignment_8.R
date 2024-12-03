library(tidyverse)
library(ggplot2)
library(skimr)
library(dplyr)
library(easystats)
library(modelr)
library(broom)
library(caret)


#1
mushroom.dat = read_csv("mushroom_growth.csv")
view(mushroom.dat)
skim(mushroom.dat)
head(mushroom.dat)


#2
mushroom.dat %>%
  ggplot(aes(x = Temperature, y = GrowthRate)) +
  geom_point() +
  labs(title = "GrowthRate vs Temperature")

mushroom.dat %>%
  ggplot(aes(x = Humidity, y = GrowthRate)) +
  geom_point() +
  labs(title = "GrowthRate vs Humidity")

mushroom.dat %>%
  ggplot(aes(x = Light, y = GrowthRate)) +
  geom_point() +
  labs(title = "GrowthRate vs Light")

mushroom.dat %>%
  ggplot(aes(x = Nitrogen, y = GrowthRate)) +
  geom_point() +
  labs(title = "GrowthRate vs Nitrogen")


#3
mod1 = glm(data = mushroom.dat,
           formula = GrowthRate ~ Temperature + Nitrogen)
mod2 = glm(data = mushroom.dat,
           formula = GrowthRate ~ Temperature + Humidity + Nitrogen)
mod3 = glm(data = mushroom.dat,
           formula = GrowthRate ~ Temperature + Humidity + Light)
mod4 = glm(data = mushroom.dat,
           formula = GrowthRate ~ Temperature + Humidity + Light + Nitrogen)
mod5 = glm(data = mushroom.dat,
           formula = GrowthRate ~ Temperature * Light)
mod6 = glm(data = mushroom.dat,
           formula = GrowthRate ~ Temperature * Humidity + Nitrogen)

summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)
summary(mod5)
summary(mod6)


#4
mse_mod1 <- mean(residuals(mod1)^2)
print(mse_mod1)  ##9631.201

mse_mod2 <- mean(residuals(mod2)^2)
print(mse_mod2)  ##7757.249

mse_mod3 <- mean(residuals(mod3)^2)
print(mse_mod3)  ##5736.752

mse_mod4 <- mean(residuals(mod4)^2)
print(mse_mod4)  ##5731.211

mse_mod5 <- mean(residuals(mod5)^2)
print(mse_mod5)  ##7572.373

mse_mod6 <- mean(residuals(mod6)^2)
print(mse_mod6)  ##7612.022


#5
which.min(c(mse_mod1, mse_mod2, mse_mod3, mse_mod4, mse_mod5, mse_mod6))
## According to this the best model is mod4 because it has the smallest MSE. 

compare_performance(mod1, mod2, mod3, mod4, mod5, mod6)
## According to this the best model is either mod3 or mod4.

compare_performance(mod1, mod2, mod3, mod4, mod5, mod6) %>% plot()
## According to this the best model was mod3 because it has the largest area. 

## I'm going to go with the best model was mod3 based on the 
## compare_performance function. 


#6
new.data = data.frame(Nitrogen = c(0, 35, 15),
                      Temperature = c(25, 30, 35),
                      Humidity = c("Low", 'High', 'High'),
                      Light = c(15, 20, 12))

new.data$PredictedGrowthRate = predict(mod3, new.data)

view(new.data)


#7
ggplot(mushroom.dat, aes(x = GrowthRate, y = predict(mod3))) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "turquoise") +
  labs(title = "Predicted vs Actual GrowthRate", x = "Actual GrowthRate", y = "Predicted GrowthRate")
