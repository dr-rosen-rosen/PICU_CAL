### Main project file

library(config)
library(tidyverse)
library(reticulate)
library(here)

debuggingState(on=FALSE)
# start ve with: source python3/bin/activate in project folder
Sys.setenv(R_CONFIG_ACTIVE = "default") # 'default')#
config <- config::get()
Sys.setenv(RETICULATE_PYTHON = config$py_version)
reticulate::source_python('1_funcs.py')
source(here('1_funcs.R'), echo = TRUE)
#source(here('2a_connect.R'), echo = TRUE)


#################################################################
############ E4 Processing Scripts
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
)

get_RTLS(
  db_loc = config$RTLS_db_loc,
  db_name = config$RTLS_db_name,
  tracking_file_loc = config$tracking_file_loc,
  tracking_file = config$tracking_file
  
)

