library(tidyverse)
library(janitor)

# DOHMH New York City Restaurant Inspection Results
# Data source:
# https://data.cityofnewyork.us/Health/DOHMH-New-York-City-Restaurant-Inspection-Results/43nn-pn8j

restaurants_raw <- read_csv("https://data.cityofnewyork.us/api/views/43nn-pn8j/rows.csv")

restaurants <-
  restaurants_raw %>% 
  clean_names() %>%
  # Keep only important variables
  select(camis, dba, boro, cuisine_description, critical_flag, score, grade) %>% 
  # Drop missing data
  drop_na() %>% 
  # Keep only garade a,b,c restaurants
  filter(grade %in% c("A", "B", "C")) %>% 
  # Lump together less frequent business types into oter category
  mutate(cuisine_description = fct_lump(cuisine_description, prop = .02)) %>% 
  filter(boro != "0")

restaurants

write_csv(restaurants, "data/nyc_restaurants.csv")

# Create codebook skeleton
# tibble(variable = names(restaurants)) %>% 
#   write_tsv("codebooks/nyc_restaurants_codebook.txt")
