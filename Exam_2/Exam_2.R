library(tidyverse)
library(ggplot2)
library(readxl)
library(janitor)
library(skimr)
library(easystats)



#1
unicef.dat = read.csv("unicef-u5mr.csv")
view(unicef.dat)
skim(unicef.dat)
names(unicef.dat)



#2
clean_names(unicef.dat)
clean.dat = unicef.dat %>%
  pivot_longer(starts_with('U5MR.'),
               names_to = 'Year',
               values_to = 'U5MR') %>%
  mutate(Year = as.integer(str_remove(Year, "U5MR\\.")))
view(clean.dat)
head(clean.dat)



#3 & 4
RASMUSSEN_Plot_1.png = clean.dat %>%
  ggplot(aes(x = Year,
             y = U5MR)) +
  geom_line() +
  facet_wrap('Continent')
RASMUSSEN_Plot_1.png
ggsave('plot_1.png', plot = RASMUSSEN_Plot_1.png)



#5 & 6
mean_U5MR = clean.dat %>%
  group_by(CountryName, Continent, Year) %>%
  summarize(mean.U5MR = mean(U5MR, na.rm = TRUE))

RASMUSSEN_Plot_2.png = mean_U5MR %>%
  ggplot(aes(x = Year,
             y = mean.U5MR,
             color = Continent)) +
  labs(y = 'Mean_U5MR') +
  geom_line() +
  theme_bw()
RASMUSSEN_Plot_2.png
ggsave('plot_2.png', plot = RASMUSSEN_Plot_2.png)




#7
mod.1 = glm(data = clean.dat,
           formula = U5MR ~ Year)

mod.2 = glm(data = clean.dat,
           formula = U5MR ~ Year + Continent)

mod.3 = glm(data = clean.dat,
           formula = U5MR ~ Year * Continent)

clean.dat$mod1 = predict(mod.1, clean.dat)
clean.dat$mod2 = predict(mod.2, clean.dat)
clean.dat$mod3 = predict(mod.3, clean.dat)



#8
compare_performance(mod.1, mod.2, mod.3)
compare_performance(mod.1, mod.2, mod.3) %>% plot
## I think mod3 is the best because it has the smallest AIC while having the
## biggest R2. In the plot it also shows that mod3 is the best. 



#9
mod1.plot = clean.dat %>%
  ggplot(aes(x = Year,
             y = mod1)) +
  geom_smooth(method = 'glm') +
  labs(y = 'Predicted U5MR') +
  theme_bw()
mod1.plot

mod2.plot = clean.dat %>%
  ggplot(aes(x = Year,
             y = mod2,
             color = Continent)) +
  geom_smooth(method = 'glm') +
  labs(y = 'Predicted U5MR') +
  theme_bw()
mod2.plot

mod3.plot = clean.dat %>%
  ggplot(aes(x = Year,
             y = mod3,
             color = Continent)) +
  geom_smooth(method = 'glm') +
  labs(y = 'Predicted U5MR') +
  theme_bw()
mod3.plot


clean.dat %>%
  pivot_longer(starts_with('mod')) %>%
  ggplot(aes(x = Year, y = value, color = Continent)) +
  geom_smooth() +
  labs(title = 'Model Predictions',
       x = 'Year',
       y = 'Predicted U5MR') +
  facet_wrap(~name) +
  theme_bw()



#10
bonus.data = data.frame(Year = 2020, Region = c("Americas"))
view(bonus.data)

pred.mod3 <- predict(mod3, bonus.data)


