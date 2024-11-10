library(tidyverse)
library(ggplot2)
library(janitor)
library(skimr)
library(dplyr)
library(easystats)

religion.dat = read.csv("C:/Users/halli/Downloads/Data_Course_RASMUSSEN/Assignments/Assignment_7/Utah_Religions_by_County.csv")
View(religion.dat)
head(religion.dat)
colnames(religion.dat)




skim(religion.dat)

religions = c('Assemblies.of.God', 'Episcopal.Church', 'Pentecostal.Church.of.God', 'Greek.Orthodox', 'LDS', 'Southern.Baptist.Convention', 'United.Methodist.Church', 'Buddhism.Mahayana', 'Catholic', 'Evangelical', 'Muslim', 'Non.Denominational', 'Orthodox')

clean.dat = religion.dat %>%
  pivot_longer(cols = c(religions), names_to = 'Religion', values_to = 'Proportion')
view(clean.dat)




# This is me exploring (all the code until #1)
head(clean.dat)
summary(clean.dat)
str(clean.dat)
dim(clean.dat) #377 6
colnames(clean.dat)
sum(is.na(clean.dat)) #0
sapply(clean.dat, class) #shows data type for each column
cor(clean.dat[, sapply(clean.dat, is.numeric)])
summarise(clean.dat)

## I did the original clean.dat then also did clean.dat.2 because it might 
## give some better variables for coding/graphing. We'll see what I end up
## using. Probably a bit of both. 
clean.dat.2 = religion.dat %>%
  pivot_longer(cols = c(religions), names_to = 'Religion', values_to = 'Proportion') %>%
  pivot_longer(cols = c('Religious', 'Non.Religious'), names_to = 'Religious.or.Not', values_to = 'Percent')
view(clean.dat.2)


clean.dat %>%
  ggplot(aes(x = Proportion, y = Religion, color = County)) +
  geom_point() 

clean.dat.2 %>%
  ggplot(aes(x = Proportion, y = Religion, color = County)) +
  geom_point() +
  facet_wrap(~Religious.or.Not)


modA = glm(data = clean.dat,
           formula = Pop_2010 ~ Proportion)
modA.plot = clean.dat %>%
  ggplot(aes(x = Proportion, y = Pop_2010)) +
  geom_smooth(method = 'glm')
print(modA.plot)

modB = glm(data = clean.dat,
           formula = Pop_2010 ~ Proportion * Religious)
modB.plot = clean.dat %>%
  ggplot(aes(x = Proportion, y = Pop_2010, color = factor(Religious))) +
  geom_smooth(method = 'glm')
print(modB.plot)

compare_models(modA, modB)
compare_performance(modA, modB)
compare_performance(modA, modB) %>% plot()




#1
clean.dat.2 %>%
  ggplot(aes(x = Pop_2010, y = Proportion, color = County)) +
  geom_point() +
  geom_smooth(method = "lm", color = "lightblue", se = FALSE) +
  labs(title = "Population vs. Proportion of Religious Group",
       x = "Population",
       y = "Proportion of Religious Group") +
  theme_minimal()


mod1 = glm(data = clean.dat.2, 
           formula = Pop_2010 ~ Proportion)
summary(mod1)

mod1.plot = clean.dat.2 %>%
  ggplot(aes(x = Proportion, y = Pop_2010, color = County)) +
  geom_smooth(method = 'glm')
print(mod1.plot)


cor(clean.dat.2$Pop_2010, clean.dat.2$Proportion)  # 0.00456343

## It does not look like the population of a county correlates with the 
## proportion of any specific religious group in that county.




#2
clean.dat %>%
  ggplot(aes(x = Proportion, y = Non.Religious, color = County)) +
  geom_point() +
  geom_smooth(method = "lm", color = "turquoise", se = FALSE) +
  labs(title = "Proportion of Religious Group vs. Proportion of Non-Religious People",
       x = "Proportion of Religious Group",
       y = "Proportion of Non-Religious People") +
  theme_minimal()


mod2 = glm(data = clean.dat, 
           formula = Proportion ~ Non.Religious)
summary(mod2)

mod2.plot = clean.dat %>%
  ggplot(aes(x = Proportion, y = Non.Religious)) +
  geom_smooth(method = 'glm')
print(mod2.plot)


cor(clean.dat$Proportion, clean.dat$Non.Religious)  #-0.04396084

## It looks like the proportion of any specific religion in a given county 
## could be slightly correlated with the proportion of non-religious people.


