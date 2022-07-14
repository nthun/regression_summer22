library(tidyverse)

sleep_raw <- read_csv2("https://raw.githubusercontent.com/nthun/performance_sleep_quality/master/data/alldata_Englishvars.csv")

sleep <- 
  sleep_raw %>% 
  janitor::clean_names() %>%  #glimpse() 
  filter(study == 1) %>% 
  transmute(id = as.numeric(subj_id),
         sex = recode(sex, `2` = "Famale", `1` = "Male"), 
         age, meq_all,
         acc_avg, acc_gs, cs_avg, highrer_order_acc, highrer_order_rt, wcst_pers_error,
         trip_learn_all_rt, trip_learn_all_acc, rt_avg, rt_gs_1min4, stat_learn_acc, stat_learn_rt,
         ais, gsqs, psqi = psqi_dist_score) 

write_csv(sleep, "data/sleep_exec.csv")
