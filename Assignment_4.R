#Assignment 4

library(tidyverse)
library(readxl)
wolf.dat = read_xlsx("C:/Users/halli/Downloads/Data_Course_RASMUSSEN/Data/Wolf_Data.xlsx",skip = 2)
view(wolf.dat)

wolf.dat %>%
  ggplot(aes(x = `Age in Years`,
             y = `BRKN TEETH`,
             colour = Sex)) +
  geom_point(size = 2, alpha = 0.6) +
  labs(x = 'Age (Years)',
       y = 'Broken Teeth') +
  theme_bw()