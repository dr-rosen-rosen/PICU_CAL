### Main project file

library(config)
library(tidyverse)
library(reticulate)
library(here)
library(readxl)
library(ggplot2)
library(plotly)
library(patchwork)
library(hrbrthemes)
library(viridis)

debuggingState(on=FALSE)
# start ve with: source python3/bin/activate in project folder
Sys.setenv(R_CONFIG_ACTIVE = "mike") # 'default')#
config <- config::get()
Sys.setenv(RETICULATE_PYTHON = config$py_version)
reticulate::source_python('1_funcs.py')
source(here('1_funcs.R'), echo = TRUE)
source(here('2_plotting_funcs.R'), echo = TRUE)
#source(here('2a_connect.R'), echo = TRUE)


#################################################################
############ Scripts for processing data for SH
#################################################################

# Load data into database, and update tracking forms
raw_E4_to_db(
  db_loc = config$E4_db_loc,
  db_name = config$E4_db_name,
  load_EDA = TRUE,
  load_HR = TRUE,
  load_ACC = TRUE,
  load_Temp = TRUE,
  load_IBI = TRUE,
  load_BVP = TRUE,
  download_path = config$E4_download_path,
  tracking_file_loc = config$tracking_file_loc,
  tracking_file = config$tracking_file
)
# Pull lightly processed data for SH

get_e4_SH(
  db_loc = config$E4_db_loc,
  db_name = config$E4_db_name,
  tracking_file_loc = config$tracking_file_loc,
  tracking_file = config$tracking_file,
  download_path = config$E4_download_path,
  load_EDA = TRUE,
  load_HR = TRUE,
  load_ACC = TRUE,
  load_Temp = TRUE,
  load_IBI = TRUE,
  load_BVP = TRUE
)

rtls_df <- get_RTLS_pg(
  db_u = config$RTLS_db_u,
  db_pw = config$RTLS_db_pw,
  tracking_file_loc = config$tracking_file_loc,
  tracking_file = config$tracking_file, # tracking_file for SH handoff, analysis_file for custom pulls
  save_shifts = TRUE # if TRUE, will save csv's for each shift (for handoff to SH)
)
# rtls_df <- get_RTLS(
#   db_loc = config$RTLS_db_loc,
#   db_name = config$RTLS_db_name,
#   tracking_file_loc = config$tracking_file_loc,
#   tracking_file = config$tracking_file, # tracking_file for SH handoff, analysis_file for custom pulls
#   save_shifts = TRUE # if TRUE, will save csv's for each shift (for handoff to SH)
# )

survey_data <- get_survey_data(
  # f_loc = paste0(config$survey_f_loc,config$Survey_f_name),
  f_loc = '/Users/mrosen44/Johns Hopkins/Salar Khaleghzadegan - Project_CollectiveAllostaticLoad/PICU Data Collection/PICU surveys/PICU_RawSurveyData.csv',
  write_file = FALSE
) %>%
  filter(shift_day != 'P6') %>%
  mutate(study_member_id = as.integer(study_member_id),
         time_point = as.integer(time_point)-1#,
         #shift_day = as.integer(shift_day)
         ) %>%
  filter(time_point > 0)

skimr::skim(survey_data)

rtls_metrics <- read.csv(here('data','rtls_metrics.csv')) %>%
  mutate(shift_chunk = shift_chunk + 1) %>%
  rename(time_point = shift_chunk) %>%
  select(shift_day, study_member_id, time_point, duration_min, entropy, fano_factor, min_in_pt_rm, prop_time_in_pt_rm) %>%
  filter(shift_day != 'Pilot_Day_6') %>%
  mutate(shift_day = map_chr(str_split(shift_day, "_"), last),
         shift_day = as.numeric(shift_day))
  # mutate(shift_day = str_split_1(shift_day,'_')[[2]])
skimr::skim(rtls_metrics)

survey_data <- survey_data %>% 
  mutate(shift_day = as.numeric(shift_day)) %>%
  full_join(rtls_metrics, by = c('shift_day','study_member_id','time_point'))

unob_df <- read.csv(here('data','df_physio_all.csv')) %>% #read.csv('df_all.csv') %>%
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
table(survey_data$cod)

cmb_df <- survey_data %>%
  left_join(unob_df, by = c('shift_day', 'study_member_id', 'time_point'))# %>%
  # mutate(shift_day = as.character(shift_day))

#################################################################
############ Build Networks from RTLS data
#################################################################

net_data <- prep_net_data(
  df = rtls_df[which(rtls_df$RTLS_ID == 404455),]
)

rtls_df %>%
  group_by(Shift,RTLS_ID) %>%
  group_walk(
    ~make_rtls_net_fig(
      df = .x,
      f_name = file.path(here(),paste0('output/rtls_net_figs/',.y$Shift,'_',.y$RTLS_ID,'.html')))
    )


#################################################################
############ Interim effort at synchronies w/ mostly python code
#################################################################
tracking_df <- read_excel(paste0(config$tracking_file_loc, config$tracking_file))

reticulate::source_python('1_funcs.py')

current_measure <- 'EDA'
test_EDA <- tracking_df[which(tracking_df$am_or_pm == 'am'),] %>% #tracking_df[which(tracking_df$shift_day == 'Shift_40'),] %>% 
  group_by(shift_day) %>%
  group_map(~ get_synchronies_py(
    shift_df = .x,
    shift_num = .y,
    measure = current_measure,
    sync_metrics = config$sync_metrics,
    sync_sampling_freq = config$sync_sampling_freq,
    sync_offset = config$sync_offset,
    sync_use_residuals = config$sync_use_residuals,
    sync_corr_method = config$sync_corr_method)
    )
team_data <- lapply(test_EDA, clean_up_synch_results, df_name = 'shift_data')
team_data <- do.call("rbind",team_data) %>%
  rename(shift_day = shift)

test_x <- lapply(test_EDA, clean_up_synch_results, df_name = 'ind_data')
test_2_EDA <- do.call("rbind",test_x) %>%
  rename(e4_id = part_id) %>%
  rename(shift_day = shift) %>%
  left_join(tracking_df[c('e4_id','shift_day','study_member_id')]) %>%
  select(-e4_id) %>%
  left_join(team_data)



### Reformat to merge with survvey data

e4_df_EDA <- test_2_EDA %>%
  # rename(study_member_id = part_id) %>%
  mutate(study_member_id = as.integer(study_member_id)) %>%
  filter(time_period != 'all_shift') %>% # dropping measures for all shift for now
  rename(time_point = time_period) %>%
  mutate(time_point = as.integer(gsub("block_","",time_point))) %>%
  mutate(time_point = time_point + 1) %>% # ??
  # rename(shift_day = shift) %>%
  mutate(shift_day = gsub("Pilot_Day_","P",shift_day)) %>%
  mutate(shift_day = gsub("Shift_0","",shift_day)) %>%
  mutate(shift_day = gsub("Shift_","",shift_day)) %>%
  pivot_wider(names_from = c(measure,metric), values_from = value) %>%
  rename_with(.fn = ~paste0(current_measure,'_',.), starts_with('Se')) %>%
  rename_with(.fn = ~paste0(current_measure,'_',.), starts_with('prop_missing')) 

e4_survey_df <- survey_data %>%
  full_join(e4_df_HR) %>%
  full_join(e4_df_EDA)
write.csv(e4_survey_df,'PICU_full_data.csv')