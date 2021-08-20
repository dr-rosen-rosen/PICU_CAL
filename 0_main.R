### Main project file

library(config)
library(tidyverse)
library(reticulate)
library(here)
library(readxl)

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
  tracking_file = config$analysis_file, # tracking_file for SH handoff
  save_shifts = FALSE # if TRUE, will save csv's for each shift (for handoff to SH)
)

survey_data <- get_survey_data(
  f_loc = paste0(config$survey_f_loc,config$Survey_f_name),
  write_file = FALSE
)

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

#write_csv(net_data$nodes,path = '522400.csv')
#################################################################
############ TEST for synchronies
#################################################################
reticulate::source_python('1_funcs.py')
get_synchronies(
  tracking_df = readxl::read_excel(paste0(config$tracking_file_loc,config$tracking_file))
)
