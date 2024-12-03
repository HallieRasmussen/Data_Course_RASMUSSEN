library(ggplot2)
library(tidyverse)
library(modelr)
library(dplyr)
library(janitor)
library(skimr)
library(broom)


#1
data = read.csv("/Users/halli/Downloads/Data_Course_RASMUSSEN/Exam_3/FacultySalaries_1995.csv")
view(data)
skim(data)
clean_names(data)

keepers = c('UnivName', 'State', 'Tier', 'AvgFullProfSalary',
            'AvgAssocProfSalary', 'AvgAssistProfSalary')

sal.dat = data %>%
  select(all_of(keepers)) %>%
  pivot_longer(starts_with('Avg'),
               names_to = 'Rank',
               values_to = 'AvgProfSalary') %>%
  mutate(Rank = case_when(Rank == 'AvgFullProfSalary' ~ 'Full',
                          Rank == 'AvgAssocProfSalary' ~ 'Assoc',
                          Rank == 'AvgAssistProfSalary' ~ 'Assist'))
view(sal.dat)

sal.plot = sal.dat %>%
  filter(Tier != "VIIB") %>%
  ggplot(aes(x = Rank, y = AvgProfSalary, fill = Rank)) +
  facet_wrap(~Tier, ncol = 3) +
  geom_boxplot() +
  theme_minimal() +
  labs(y = 'Salary') +
  theme(axis.text.x = element_text(angle = 65))
sal.plot



#2
summary(sal.dat)
anova = aov(data = sal.dat,
            formula = AvgProfSalary ~ State + Rank + Tier)
print(anova)
summary(anova)



#3
data2 = read.csv("/Users/halli/Downloads/Data_Course_RASMUSSEN/Exam_3/Juniper_Oils.csv")
view(data2)
skim(data2)
clean_names(data2)

keepers2 = c('Tree_Species', 'BurnYear', 'YearsSinceBurn', "alpha.pinene","para.cymene",
             "alpha.terpineol","cedr.9.ene","alpha.cedrene","beta.cedrene", "cis.thujopsene",
             "alpha.himachalene","beta.chamigrene", "cuparene","compound.1","alpha.chamigrene",
             "widdrol","cedrol", "beta.acorenol","alpha.acorenol","gamma.eudesmol",
             "beta.eudesmol","alpha.eudesmol","cedr.8.en.13.ol", "cedr.8.en.15.ol","compound.2",
             "thujopsenal")
chemicals =  c("alpha.pinene","para.cymene","alpha.terpineol","cedr.9.ene", "alpha.cedrene",
               "beta.cedrene","cis.thujopsene", "alpha.himachalene","beta.chamigrene",
               "cuparene","compound.1", "alpha.chamigrene","widdrol","cedrol","beta.acorenol",
               "alpha.acorenol","gamma.eudesmol","beta.eudesmol", "alpha.eudesmol",
               "cedr.8.en.13.ol","cedr.8.en.15.ol", "compound.2","thujopsenal")

jun.dat = data2 %>%
  select(all_of(keepers2)) %>%
  pivot_longer(all_of(chemicals),
               names_to = 'ChemicalID',
               values_to = 'Concentration')
view(jun.dat)



#4
jun.plot = jun.dat %>%
  ggplot(aes(x = YearsSinceBurn, y = Concentration)) +
  facet_wrap(~ChemicalID, scales = 'free') +
  geom_smooth() +
  theme_minimal()
jun.plot



#5
glm_mod.jun = glm(data = jun.dat,
                  formula = Concentration ~ YearsSinceBurn * ChemicalID, 
                  family = gaussian())
tidy_mod.jun = tidy(glm_mod.jun)
sig_mod.jun = tidy_mod.jun %>% 
  filter(p.value < 0.05) %>%
  select(term, estimate, std.error, statistic, p.value)
print(sig_mod.jun)





