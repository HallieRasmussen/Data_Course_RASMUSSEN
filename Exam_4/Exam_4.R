
# I am redoing Exam 1 (in my repository it's just called Exam)


#1
library(tidyverse)
df = read.csv("./cleaned_covid_data.csv")
view(df)
head(df)


#2
?grepl

A_states = subset(df, grepl('^A',df$Province_State))
view(A_states)


#3
library(ggplot2)
?loess
?geom_smooth()

A_states %>%
  ggplot(aes(x = Last_Update,
             y = Deaths)) +
  geom_point(color = 'darkgreen') +
  geom_smooth(method = 'loess',se = FALSE) +
  facet_wrap(~Province_State, scales = 'free')


#4
state_max_fatality_rate = df %>%
  group_by(Province_State) %>%
  summarize(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm = TRUE)) %>%
  arrange(desc(Maximum_Fatality_Ratio))
view(state_max_fatality_rate)


#5
?geom_bar()
?factor

state_max_fatality_rate %>%
  ggplot(aes(x = factor(Province_State, levels = Province_State),
             y = Maximum_Fatality_Ratio)) +
  geom_col(color = 'black', fill = 'darkgreen') +
  labs(title = 'Province State vs Max Fatality Ratio',
       x = 'Province State',
       y = 'Max Fatality Ratio') +
  theme(axis.text.x = element_text(angle = 90))


#6 (bonus)
library(dplyr)
?dplyr
?group_by
?summarize

deaths.df = df %>%
  group_by(Last_Update) %>%
  summarise(cumulative_deaths = sum(Deaths))
view(deaths.df)

deaths.df %>%
  ggplot(aes(x = Last_Update,
             y = cumulative_deaths)) +
  geom_point(color = 'darkgreen') +
  labs(title = 'Cumulative Deaths in the Entire US Over Time',
       x = 'Time',
       y = 'Cumulative Deaths')


#I added some color to the graphs for fun
