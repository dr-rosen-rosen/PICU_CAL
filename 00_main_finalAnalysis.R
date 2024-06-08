### Main project file for final anlaysis / paper

library(tidyverse)
library(here)

debuggingState(on=FALSE)
Sys.setenv(R_CONFIG_ACTIVE = "mike") # 'default')#
config <- config::get()
source(here('1_funcs.R'), echo = TRUE)
# source(here('2_plotting_funcs.R'), echo = TRUE)



##################################################################
##################################################################
######### Read in all files
##################################################################
##################################################################


survey_data <- get_survey_data(
  # f_loc = paste0(config$survey_f_loc,config$Survey_f_name),
  # f_loc = '/Users/mrosen44/Johns Hopkins/Salar Khaleghzadegan - Project_CollectiveAllostaticLoad/PICU Data Collection/PICU surveys/PICU_RawSurveyData.csv',
  f_loc = here::here('data','PICU_RawSurveyData.csv'),
  write_file = FALSE
) |>
  filter(shift_day != 'P6') %>%
  mutate(study_member_id = as.integer(study_member_id),
         time_point = as.integer(time_point)-1#,
         #shift_day = as.integer(shift_day)
  ) |>
  filter(time_point > 0)

skimr::skim(survey_data)

rtls_metrics <- read.csv(here('data','rtls_metrics.csv')) |>
  mutate(shift_chunk = shift_chunk + 1) |>
  rename(time_point = shift_chunk) |>
  select(shift_day, study_member_id, time_point, duration_min, entropy, fano_factor, min_in_pt_rm, prop_time_in_pt_rm) %>%
  filter(shift_day != 'Pilot_Day_6') |>
  mutate(shift_day = map_chr(str_split(shift_day, "_"), last),
         shift_day = as.numeric(shift_day))
# mutate(shift_day = str_split_1(shift_day,'_')[[2]])
skimr::skim(rtls_metrics)

survey_data <- survey_data %>% 
  mutate(shift_day = as.numeric(shift_day)) %>%
  full_join(rtls_metrics, by = c('shift_day','study_member_id','time_point'))

# Physio files, processed using the tpd project
#unob_df <- read.csv(here('data','PICU_df_physio_all_10-30-2023.csv')) %>% 
unob_df <- read.csv(here('data','PICU_df_physio_all_06-07-2024.csv')) %>% 
  filter(shift_day != 'Pilot_Day_6') %>%
  mutate(
    time_point = shift_chunk + 1,
    shift_day = as.numeric(str_remove(shift_day, 'Shift_'))
  ) %>%
  select(-c(X,e4_id, rtls_id, sociometric_id, rhythm_badge_num, shift_chunk, duration_min, task_num)) %>%
  mutate(
    code_event = replace_na(code_event, replace = 0)
  )
skimr::skim(unob_df)
# table(survey_data$cod)

cmb_df <- survey_data %>%
  left_join(unob_df, by = c('shift_day', 'study_member_id', 'time_point'))# %>%
# mutate(shift_day = as.character(shift_day))
skimr::skim(cmb_df)
