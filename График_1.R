library(tidyverse)
library(haven)

data %>%
  drop_na(o26_1) %>% 
  select(id_w, o26_1, o26_2, o26_3) %>%
  filter(id_w == 2024) %>% 
  filter(!o26_1 %in% c(99999996, 99999997, 99999998, 99999999)) %>%
  select(o26_1) %>% 
  unique()

data %>% 
  mutate(o26_1 = as_factor(o26_1)) %>% 
  count(o26_1, sort = TRUE, prop = TRUE)


result <- data %>% 
  mutate(o26_1 = as_factor(o26_1)) %>%  
  count(o26_1, sort = TRUE) %>% 
  mutate(share = n / sum(n))

sum(result$n)          # должно совпадать с числом строк без NA
sum(result$share) 

sum(as_factor(data$o26_1) == "НА СТАРОСТЬ", na.rm = TRUE)
