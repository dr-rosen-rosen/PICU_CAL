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

rtls_df <- get_RTLS(
  db_loc = config$RTLS_db_loc,
  db_name = config$RTLS_db_name,
  tracking_file_loc = config$tracking_file_loc,
  tracking_file = config$tracking_file, # tracking_file for SH handoff, analysis_file for custom pulls
  save_shifts = TRUE # if TRUE, will save csv's for each shift (for handoff to SH)
)

survey_data <- get_survey_data(
  f_loc = paste0(config$survey_f_loc,config$Survey_f_name),
  write_file = FALSE
) %>%
  mutate(study_member_id = as.integer(study_member_id)) %>%
  mutate(time_point = as.integer(time_point))

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

measure <- 'HR'
test_HR <- tracking_df %>%#tracking_df[which(tracking_df$shift_day == 'Shift_40'),] %>%
  group_by(shift_day) %>%
  group_map(~ get_synchronies_py(
    shift_df = .x,
    shift_num = .y,
    measure = measure,
    sync_metrics = config$sync_metrics,
    sync_sampling_freq = config$sync_sampling_freq,
    sync_offset = config$sync_offset,
    sync_use_residuals = config$sync_use_residuals,
    sync_corr_method = config$sync_corr_method)
    )
test_x <- lapply(test_HR, clean_up_synch_results, df_name = 'ind_data')
test_2_HR <- do.call("rbind",test_x)

team_data <- lapply(test_HR, clean_up_synch_results, df_name = 'team_data')


### Reformat to merge with survvey data

e4_df_HR <- test_2_HR %>%
  rename(study_member_id = part_id) %>%
  mutate(study_member_id = as.integer(study_member_id)) %>%
  filter(time_period != 'all_shift') %>% # dropping measures for all shift for now
  rename(time_point = time_period) %>%
  mutate(time_point = as.integer(gsub("block_","",time_point))) %>%
  mutate(time_point = time_point + 1) %>%
  rename(shift_day = shift) %>%
  mutate(shift_day = gsub("Pilot_Day_","P",shift_day)) %>%
  mutate(shift_day = gsub("Shift_0","",shift_day)) %>%
  mutate(shift_day = gsub("Shift_","",shift_day)) %>%
  pivot_wider(names_from = c(measure,metric), values_from = value)

e4_survey_df <- e4_df %>%
  full_join(survey_data)
e4_survey_df <- e4_survey_df %>%
  full_join(e4_df_HR)



#################################################################
############ Create data visuals for Fellows talk
#################################################################
reticulate::source_python('1_funcs.py')
tracking_df <- read_excel(paste0(config$tracking_file_loc, config$tracking_file))

measure <- 'ACC'
test <- tracking_df[which(tracking_df$shift_day == 'Shift_03'),] %>%
  group_by(shift_day) %>%
  group_map(~ get_synchronies(.x,.y, measure)) %>% # this says 'get_synchronies' but is just getting raw data
  do.call("rbind", .)
colnames(test) <- lapply(colnames(test), rename_func)

### renaming for charts

# shift 3:
test <- test %>%
  rename(
    tm_1 = id_1266,
    tm_2 = id_2118,
    tm_3 = id_2487,
    tm_4 = id_2802
  )

test <- test %>%
  pivot_longer(!TimeStamp, names_to = 'indvidual', values_to = 'value')
test$indvidual <- ordered(test$indvidual, levels = c('tm_1', 'tm_2', 'tm_3','tm_4'))

code_time <- lubridate::ymd("2020-11-19")
lubridate::tz(code_time) <- "America/New_york"
lubridate::hour(code_time) <- 8
lubridate::minute(code_time) <- 27

code_end <- lubridate::ymd("2020-11-19")
lubridate::tz(code_end) <- "America/New_york"
lubridate::hour(code_end) <- 9
lubridate::minute(code_end) <- 19

# t_breaks <- lubridate::hms(c('07:00:00', '11:00:00', '15:00:00','19:00:00'))
# my_time <- lubridate::ymd("2020-11-19")
# lubridate::tz(my_time) <-"America/New_york"
# t_breaks <- t_breaks + my_time
p_HR <- ggplot(test, aes(x=TimeStamp, y=value)) + #, colour = indvidual, group = indvidual
  geom_line() + 
  xlab("") +
  facet_grid(indvidual ~ .) +
  annotate("rect", xmin = code_time, xmax = (code_end), ymin = min(test$value,na.rm = TRUE), ymax = Inf,
           alpha = .2, fill = 'red') +
  
  ylab('Cardiac Activity \n (Avg HR)') +
  #scale_x_datetime(breaks = lubridate::hms(t_breaks)) +
  theme_light()
p_EDA <- ggplot(test, aes(x=TimeStamp, y=value)) + #, colour = indvidual, group = indvidual
  geom_line() + 
  xlab("") +
  facet_grid(indvidual ~ .) +
  annotate("rect", xmin = code_time, xmax = (code_end), ymin = min(test$value,na.rm = TRUE), ymax = Inf,
           alpha = .2, fill = 'red') +
  ylab('Electro Dermal Activity \n (MicroS)') +
  #scale_x_datetime(breaks = lubridate::hms(t_breaks)) +
  theme_light()


test$indvidual <- factor(test$indvidual, levels = rev(levels(test$indvidual)))
hm_df <- test %>%
  mutate(time_interval = lubridate::round_date(TimeStamp, "5 mins")) %>%
  group_by(indvidual, time_interval) %>%
  summarize(AvgEnergy = mean(value)) %>%
  ungroup()

rtls_hm_df <- rtls_df %>%
  mutate(Time_In = lubridate::force_tz(Time_In,"UTC")) %>%
  mutate(Time_In = lubridate::with_tz(Time_In, tzone = "America/New_York")) %>%
  mutate(time_interval = lubridate::round_date(Time_In, "5 mins")) %>%
  mutate(RTLS_ID = as.factor(RTLS_ID)) %>%
  group_by(RTLS_ID, time_interval) %>%
  summarize(Transition_count = n()) %>%
  ungroup()

trans_hm_p <- ggplot(rtls_hm_df, aes(x=time_interval)) +
  #geom_tile(aes(fill = Transition_count), colour = "white", na.rm = TRUE) +
  geom_bar(stat = "count") +
  facet_grid(RTLS_ID ~ .) +
  theme_light() #+
  #scale_fill_viridis(discrete=FALSE) 
  

hm_p <- ggplot(hm_df, aes(x=time_interval, y = indvidual)) +
  geom_tile(aes(fill = AvgEnergy), colour = "white", na.rm = TRUE)  +
  # annotate("rect", xmin = code_time, xmax = (code_time+1800), ymin = min(test$value,na.rm = TRUE), ymax = Inf,
  #          alpha = .2, fill = 'red') +
  theme_light() +
  scale_fill_viridis(discrete=FALSE) +
  #scale_y_discrete(position = "right") + 
  ylab("Accelerometer Movement Metrics")
  #theme(legend.position="none")
hm_p

p_EDA / p_HR / hm_p