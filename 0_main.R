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

