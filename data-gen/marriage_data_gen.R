library(tidyverse)
library(haven)

# Load data
marriage_raw <- read_dta("https://data.princeton.edu/pop509/divorce3.dta")

# Transform data
marriage <- 
  marriage_raw %>% 
  filter(marnum == 1) %>% 
  mutate(divorced = abs(censor - 1), .after = spans) %>% 
  select(id:divorced, hiseduc:agediff, -weight, -marnum) %>% 
  rename(time = spans, 
         hisage = age)

# Write data
write_csv(marriage, "data/marriage.csv")

# Create codebook skeleton

# tibble(variable = names(marriage), description = NULL) %>% 
#   write_csv("codebooks/marriage.txt")

  

