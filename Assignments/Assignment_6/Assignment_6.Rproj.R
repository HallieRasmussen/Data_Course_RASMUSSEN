library(tidyverse)
library(ggplot2)
library(skimr)
library(gganimate)
dat = read_csv("../../Data/BioLog_Plate_Data.csv")
view(dat)
skim(dat)



#1
clean.dat = dat %>%
  pivot_longer(starts_with('Hr'),
               names_to = 'Time_in_Hrs',
               values_to = 'Absorbance') %>%
  mutate(Time_in_Hrs = case_when(Time_in_Hrs == 'Hr_24' ~ 24,
                                 Time_in_Hrs == 'Hr_48' ~ 48,
                                 Time_in_Hrs == 'Hr_144' ~144))
view(clean.dat)



#2
sample.type = clean.dat %>%
  mutate(Sample_Type = case_when(`Sample ID` == 'Clear_Creek' ~ 'Water',
                                 `Sample ID` == 'Soil_1' ~ 'Soil',
                                 `Sample ID` == 'Waste_Water' ~ 'Water',
                                 `Sample ID` == 'Soil_2' ~ 'Soil'))
view(sample.type)



#3
sample.type %>%
  filter(Dilution == 0.1) %>%
  ggplot(aes(x = Time_in_Hrs,
             y = Absorbance,
             color = Sample_Type)) +
  geom_line() +
  facet_wrap('Substrate') +
  labs(x = 'Time',
       y = 'Absorbance',
       color = 'Type') 



#4
absorbance_values = clean.dat %>%
  filter(Substrate == 'Itaconic Acid') %>%
  group_by(`Sample ID`, Rep, Time_in_Hrs, Dilution) %>%
  summarize(absorbance_values = mean(Absorbance, na.rm = TRUE))

Mean_absorbance = absorbance_values %>%
  group_by(`Sample ID`, Time_in_Hrs, Dilution) %>%
  summarise(Mean_absorbance = mean(absorbance_values, na.rm = T))
view(Mean_absorbance)


animation_plot = Mean_absorbance %>%
  ggplot(aes(x = Time_in_Hrs,
             y = Mean_absorbance,
             color = `Sample ID`)) +
  geom_line() +
  facet_wrap('Dilution') +
  labs(x = 'Time',
       y = 'Mean_absorbance',
       color = 'Type') +
  transition_reveal(Time_in_Hrs)
animate(animation_plot, nframes = 100, fps = 10)
