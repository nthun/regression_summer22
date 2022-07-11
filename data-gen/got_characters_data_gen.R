# devtools::install_github("MangoTheCat/GoTr")

library(GoTr) # Access of ASOIF API

got_characters <- 
  map(1:50, ~got_api(type = "characters", 
                     query = list(page = .x, pageSize = "50"))) %>%
  unlist(recursive = FALSE)

# Put list data into nested data frame
# Data rectangling
got_df <-
  got_characters %>%
  tibble( # Create an analyzable data frame from the complex list object
    url = map_chr(., "url"),
    id = str_extract(url, "\\d+") %>% as.integer(),
    name = map_chr(., "name"),
    gender = map_chr(., "gender"),
    culture = map_chr(., "culture"),
    born = map_chr(., "born"),
    died = map_chr(., "died"),
    titles = map(., "titles"),
    aliases = map(., "aliases"),
    spouse = map_chr(., "spouse"),
  ) %>% 
  select(-1) %>% 
  mutate_if(is.character,
            ~if_else(str_trim(.) == "", NA_character_, .)) %>% # Explicit NA
  rowwise() %>% 
  mutate(
    alias_length = length(aliases), # How many aliases does the person have
    alias_names = unlist(aliases) %>% paste(collapse = "; "), # Collapse aliases to one string
    has_spouse = if_else(!is.na(spouse), TRUE, FALSE), # Have a spouse?
    birth_era = str_to_upper(born) %>% str_extract("AC|BC"), # Birth and death year
    birth_year = str_extract(born, "\\d+") %>% 
      as.numeric() %>% 
      if_else(birth_era == "BC", `-`(.), .),  # If BC, make it negative
    death_era = str_extract(str_to_upper(died), "AC|BC"),
    death_year = str_extract(died, "\\d+") %>% 
      as.numeric() %>% 
      if_else(death_era == "BC", `-`(.), .), # If BC, make it negative
    birth_place = str_to_title(born) %>% # Birth and death place 
      str_extract("(?<=At ).*") %>% 
      str_remove_all("Near |Or "), 
    death_place = str_to_title(died) %>% 
      str_extract("(?<=At ).*") %>% 
      str_remove_all("Near |Or "),
    is_alive = if_else(is.na(died) & 300 - birth_year < 100, TRUE, FALSE), # Alive?
    age = if_else(is_alive, 300 - birth_year, death_year - birth_year)) %>% # Age (or age when died)
  mutate_at(vars(birth_place, death_place), ~str_remove(., "\\(.*\\)") %>% 
              str_trim()) %>%
  ungroup()

got_df %>% 
  select(-(born:aliases), -(aliases),  -ends_with("_era")) %>% names()
  write_csv("data/got_characters.csv")

# Create codebook
tibble(variable = names(got_df)) %>% 
  write_tsv("codebooks/got_characters.txt")

