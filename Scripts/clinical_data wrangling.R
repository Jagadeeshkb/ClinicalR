#---Clinical programming in R

#first observation of a by group

library(dplyr)

set.seed(123)

d <- data.frame(
  group = rep(1:3, each = 3),
  year = rep(seq(2000,2002,1),3),
  value = sample(1:9, r = T))

d %>% 
  group_by(group) %>% 
  summarise(first_value = first(na.omit(value)),
            last_value = last(na.omit(value)))


d %>% 
  group_by(group) %>%
  arrange(group,year) %>%
  mutate(id=case_when(row_number()==1~"Y",TRUE~NA))
  
