library(tidyverse)

data <- tibble(
  participant_id = paste0("id_", 1:500)
)

data <-
  data %>% 
  group_by(participant_id) %>% 
  mutate(
    religion = sample(0:9, 1, replace = TRUE, prob = c(0.1, 0.2, 0.4, 0.45, 0.5, 0.6, 0.45, 0.45, 0.2, 0.1)),
    age = round(rnorm(1, 50, 15)),
    age = rdunif(1, b = 18, a = 80),
    choice = case_when(
      age < 50 ~ rbinom(1, size = 1, prob = .63),
      age >= 50 ~ rbinom(1, size = 1, prob = .42)
    ),
    choice = case_when(
      choice == 0 ~ "safe",
      choice == 1 ~ "risky"
    ),
    choice = as.factor(choice)
  ) %>% 
  ungroup()

# check generated data
data %>% 
  mutate(
    age_group = case_when(
      age >= 50 ~ "old",
      age < 50 ~ "young"
    )
  ) %>% 
  count(age_group, choice) %>% 
  arrange(age_group)

# save data
write_csv(data, here::here("data/riskaversion_data.csv"))
