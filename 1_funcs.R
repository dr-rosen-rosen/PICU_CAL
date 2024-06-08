###############################################################################################
###############################################################################################
#####################   FUNCS for Processing and Analyzing PICU CAL data
#####################
###############################################################################################
###############################################################################################
library(readr)
library(tidyverse)
library(networkD3)
library(r2d3)
library(DBI)
library(RSQLite)
###############################################################################################
#####################   Functions for Survey data
###############################################################################################

get_survey_data <- function(f_loc, write_file) {
  
  #opening the survey sheet
  picu_data <-read_csv(f_loc)
  
  #turning character values that need to be numeric into numeric
  cols.num <- c("single_stress","trust_1","trust_2","trust_3","trust_4",
                "trust_5","trust_6","trust_7","trust_8","trust_9","trust_10",
                "trust_11","trust_12","trust_13","trust_14","trust_15",
                "trust_16","trust_17","trust_18","trust_19","trust_20",
                "trust_21","belief_1","belief_2","belief_3","belief_4",
                "belief_5","belief_6","belief_7","belief_8","belief_9",
                "belief_10","belief_11","belief_12","belief_13","belief_14",
                "belief_15","belief_16","belief_17","belief_18","mbi_1", "mbi_2", 
                "team_census", "team_ppv", "team_vaso", "team_ecmo", "team_renal", 
                "team_cardiac", "red_pm_census", "red_pm_ppv", "red_pm_ecmo", 
                "red_pm_renal", "red_pm_cardiac", "picu_census", "admit_picu", 
                "transfer_picu", "discharge_picu", "beside_nurse_picu")
  picu_data[cols.num]<- sapply(picu_data[cols.num], as.numeric)
  
  #turning numeric values that need to be character into character
  cols.chr <- c("study_member_id","time_point")
  picu_data[cols.chr]<- sapply(picu_data[cols.chr], as.character)
  
  #NASA TLX for individual items
  picu_data <- picu_data %>%
    rowwise() %>%
    mutate(tlx_individual = mean(c(tlx_1,tlx_2, tlx_3, tlx_4, tlx_5, tlx_6)))
  
  #NASA TLX for team items
  picu_data <- picu_data %>%
    rowwise() %>%
    mutate(tlx_team = mean(c(tlx_7,tlx_8, tlx_9, tlx_10, tlx_11, tlx_12)))
  
  #creating a stressors composite score
  picu_data <-picu_data %>%
    rowwise() %>%
    mutate(stressors_total = sum(c(stressors_1, stressors_2, stressors_3, stressors_4, 
                                   stressors_5, stressors_6, stressors_7, stressors_8, 
                                   stressors_9, stressors_10, stressors_11, stressors_12, 
                                   stressors_13)),
           stressors_team = sum(c(stressors_2, stressors_8, stressors_9, stressors_10, stressors_11, stressors_12)),
           stressors_ind = sum(c(stressors_7, stressors_13)),
           stressors_interpersonal = sum(c(stressors_5,stressors_6)),
           stressors_org = sum(c(stressors_1, stressors_3, stressors_4)),
           stressors_total_dich = if_else(stressors_total > 0, 1, 0),
           stressors_team_dich = if_else(stressors_team > 0, 1, 0),
           stressors_ind_dich = if_else(stressors_ind > 0, 1, 0),
           stressors_interpersonal_dich = if_else(stressors_interpersonal > 0, 1, 0),
           stressors_org_dich = if_else(stressors_org > 0, 1, 0)
    )
  
  #creating a composite score for trust in teams, propensity to trust subscale
  picu_data <- picu_data %>%
    mutate(trust_prop_t = mean(c(trust_1, trust_2, trust_3, trust_4, trust_5, trust_6)))
  #creating a composite score for trust in teams, perceived trustworthiness subscale
  picu_data <- picu_data %>%
    rowwise() %>%
    mutate(trust_perc_t = mean(c(trust_7, trust_8, trust_9, 8-trust_10, 8-trust_11, trust_12 )))
  #creating a composite score for trust in teams, cooperative behaviors subscale
  picu_data <-picu_data %>%
    rowwise() %>%
    mutate(trust_coop_beh = mean(c(trust_13, trust_14, trust_15, 8-trust_16, 8-trust_17, trust_18 )))
  #creating a composite score for trust in teams, monitoring behaviors subscale
  picu_data <- picu_data %>%
    rowwise() %>%
    mutate(trust_mon_beh = mean(c(trust_19, trust_20, trust_21)))
  #creating a composite score for team process scale, transition processes subscale
  picu_data <- picu_data %>%
    rowwise() %>%
    mutate(tps_transition = mean(c(tps_1, tps_2, tps_3)))
  #creating a composite score for team process scale, action processes subscale
  picu_data <- picu_data %>%
    rowwise() %>%
    mutate(tps_action = mean(c(tps_4, tps_5, tps_6, tps_7)))
  #creating a composite score for team process scale, interpersonal processes subscale
  picu_data <- picu_data %>%
    rowwise() %>%
    mutate(tps_interpersonal = mean(c(tps_8, tps_9, tps_10)))
  #creating a composite score for team capability belief scale, team process efficacy subscale
  picu_data <- picu_data %>%
    rowwise() %>%
    mutate(belief_process = mean(c(belief_1, belief_2, belief_3, belief_4, belief_5, belief_6, 
                                   belief_7, belief_8, belief_9, belief_10)))
  #creating a composite score for team capability belief scale, team potency subscale
  picu_data <- picu_data %>%
    rowwise() %>%
    mutate(belief_potency = mean(c(belief_11, belief_12, belief_13, belief_14, belief_15, 
                                   belief_16, belief_17, belief_18)))
  #creating a composite score for MBI scale
  picu_data <- picu_data %>%
    rowwise() %>%
    mutate(mbi = mean(c(mbi_1, mbi_2)))

  #recoding N/A to 1 in variables mayo_9:mayo_16
  # different way to change all at once; I checked it and it seems to work this way
  picu_data <- picu_data %>%
    rowwise() %>%
    mutate(across(mayo_9:mayo_16, ~recode(.,"N/A" = "1")))

  #turning character values that need to be numeric into numeric for Mayo survey
  cols.num_mayo <- c("mayo_1","mayo_2","mayo_3","mayo_4","mayo_5","mayo_6",
                     "mayo_7","mayo_8", "mayo_9","mayo_10","mayo_11","mayo_12",
                     "mayo_13","mayo_14","mayo_15","mayo_16")
  picu_data[cols.num_mayo]<- sapply(picu_data[cols.num_mayo], as.numeric)
  #creating a composite score for Mayo High Performance scale by calculating the sum of mayo_1:mayo_16
  picu_data <- picu_data %>%
    rowwise() %>%
    mutate(mayo_total = sum(c(mayo_1,mayo_2,mayo_3,mayo_4,mayo_5,mayo_6,mayo_7,
                              mayo_8, mayo_9,mayo_10,mayo_11,mayo_12,mayo_13,
                              mayo_14,mayo_15,mayo_16)))
  if (write_file) {
    #saving a csv file of the updated dataset
    write.csv(picu_data, "picu_data.csv") #if wek keep this; should make name and location definable
  }
  return(picu_data)
}

###############################################################################################
#####################   Functions for network metrics and visualization
###############################################################################################

prep_net_data <- function(df) {
  
  # This funciton takes an RTLS df and creates files for:
  #   Edges
  #   Nodes
  nodes <- df %>% 
    group_by(Receiver) %>%
    summarize(duration = sum(Duration)) %>%
    ungroup() %>%
    mutate(id = row_number() - 1) %>%
    left_join(
      distinct(df,Receiver,Receiver_recode, Receiver_name),
      by = "Receiver"
    ) %>%
    rename(rec_num = Receiver,
           type = Receiver_recode,
           description = Receiver_name) %>%
    arrange(desc(duration))
  
  #adding back
  distinct(df,Receiver,Receiver_recode)
  
  df <- relabel_nodes(df,nodes) # this just recodes the reciever id to the 'id' var from above; why is that so hard in R?
  
  edges <- df %>%
    arrange(Time_In) %>% # makes sure rows are ordered in ascending time order
    mutate(to = lead(Receiver)) %>% # creates new colum for destination link based on shifted Receiver column
    na.omit() %>% # drops last row of NA created by shifting
    rename(from = Receiver)  %>% # renames Receiver column to the 'from' end of edge
    group_by(from, to) %>%
    summarize(weight = n()) %>%
    ungroup()
  
  return(list('nodes' = nodes, 'edges' = edges))
}

make_rtls_net_fig <- function(df, f_name) {
  net_data <- prep_net_data(df)
  forceNetwork(Links = net_data$edges,
               Nodes = net_data$nodes,
               Source = "from",
               Target = "to",
               NodeID = "description",
               Group = "type",
               Value = "weight",
               Nodesize = "duration",
               opacity = 1,
               fontSize = 16,
               zoom = TRUE,
               legend = TRUE) %>%
    saveNetwork(file = f_name, selfcontained = TRUE)
}


###############################################################################################
#####################   Helper functions for Repacking data from synchrony scripts
###############################################################################################

clean_up_synch_results <- function(single_e4_df, df_name) {
  ### This takes one top level lest from teh list of lists returned by python function
  ### It separates out the individual and team components of the list
  return(reticulate::py_to_r(single_e4_df[[df_name]]))
}

clean_up_py <- function(pd_df) {
  return(reticulate::py_to_r(pd_df))
}

rename_func <- function(n) {
  if (n == 'TimeStamp') {
    return('TimeStamp')
  } else {
    return(paste0('id_',n))
  }
}


###############################################################################################
#####################   OLD Functions for E4 syncrhony done in R
#####################     Ran in to MANY issues in R w/ sqlite
#####################     attempted to address that with small py functions
#####################     more issues with timestamps, and dataframes passing back and forth
#####################     for time reasons; abandoning this for now.
#####################     This will be helpful when we have time (and are not using sqlite anymore)
###############################################################################################
create_ACC_energy_metric_r <- function(one_e4) {
  dimensions = c('x', 'y', 'z')
  print('here')
  for (dimension in dimensions) {
    one_e4[[dimension]] <- one_e4[[dimension]]^2
  }
  one_e4$energy <- rowSums(one_e4[,dimensions])
  one_e4$energy <- '^' (one_e4$energy, 1/2)
  one_e4 <- one_e4[ , !(names(one_e4) %in% dimensions)]
  print(colnames(one_e4))
  return(one_e4)
}


pull_e4_data <- function(e4_ids, shift_start, measure,e4_to_part_id) {
  print(e4_to_part_id)
  shift_stop <- shift_start + lubridate::hours(12)
  print(shift_start)
  print(shift_stop)
  df_list = list()
  all_e4_data <- TRUE
  for (e4 in e4_ids) {
    ### pulls data for given badge within shift range

    t_name <- paste0('Table_',e4,'_',measure)
    ### built this as temporary work around to issues directly filtering timestamp data in sqlite from R
    one_e4 <- pull_e4_data_py(
      # db is hard coded in script for now, since this is temporary fix.
      t_name = t_name,
      shift_start = shift_start,
      shift_stop = shift_stop
    )
    ### This SHOULD be easy to do in R, but dbplyr does not play well with sqlite for timestamps
    ### Using python script w/sqlalchemy as temporary fix until we migrat to better db solution
    # one_e4 <- tbl(e4_con, t_name) %>%
    #   #filter(TimeStamp >= shift_start & TimeStamp <= shift_stop) %>%
    #   collect()
    # one_e4 <- one_e4 %>%
    #   filter(TimeStamp >= shift_start & TimeStamp <= shift_stop)

    ### does minimal data QC... this needs exapnding
    if (nrow(one_e4) > 0) { # checks if anything is in the data, and adds to list if there is
      print('... has data!')
      print(nrow(one_e4))
      ### Need to add metric conversion for ACC (go from 3 cols to 'energy metric')
      # sets col name to part_id; This is done to track who is who once they are integrated
      # need to expand this to other measures with named list of measure / metrics
      if (measure == 'HR'){
        colnames(one_e4)[which(names(one_e4) == "AvgHR")] <- e4_to_part_id[[e4]]
      } else if (measure == 'EDA') {
        colnames(one_e4)[which(names(one_e4) == "MicroS")] <- e4_to_part_id[[e4]]
      } else if (measure == 'ACC') {
        one_e4 <- create_ACC_energy_metric_r(one_e4)
        colnames(one_e4)[which(names(one_e4) == "energy")] <- e4_to_part_id[[e4]]
        }
      df_list[[e4]] <- one_e4
    } else {
      print('... has NO data!')
      all_e4_data <- FALSE
    }
  }
  if (all_e4_data == TRUE) {
    all_data <- df_list %>% purrr::reduce(full_join, by = "TimeStamp")
    all_data$TimeStamp <- lubridate::as_datetime(all_data$TimeStamp)
    all_data$TimeStamp <- lubridate::force_tz(all_data$TimeStamp, "America/New_York") # timestamps were coming back with Batlimore Times, but marked as UTC timezone; this fixes
    print(paste('All data this big... ',ncol(all_data),' by ',nrow(all_data)))
    return(all_data)
  } else {
    return('WHOOOPS')
  }
}

make_sync_matrix <- function(e4_data){
  ### define datastructures for making and storing coef_matrix
  print(typeof(e4_data))
  working_roles <- colnames(e4_data)
  print(working_roles)
  print(typeof(working_roles))
  working_roles <- working_roles %>% purrr::list_modify("TimeStamp" = NULL)
  print(working_roles)
  Sync_Coefs <- data.frame(matrix(ncol=length(working_roles),nrow=length(working_roles), dimnames=list(working_roles, working_roles)))
  ### format and clean timeseries
  time_lag <- 50
  ### Creates Table 1 in Guastello and Perisini
  for (from_role in working_roles){
    print(from_role)
    role_acf <- e4_data %>% select(from_role) %>% drop_na() %>% acf(plot = FALSE)
    Sync_Coefs[[from_role,from_role]] <- role_acf$acf[time_lag]
  }
  return(Sync_Coefs)
}

get_synchronies <- function(shift_df, shift, measure) {
  e4_ids <- unique(shift_df$e4_id)
  shift_date <- unique(shift_df$date)[1] # should add a check to make sure they are all the same
  lubridate::tz(shift_date) <- "America/New_york" # changing to EDT; E4 data is in ETC... need to check that daylight savings is handled correctly
  am_or_pm <- unique(shift_df$am_or_pm)[1] # should add a check to make sure they are all the same
  if (am_or_pm == 'am') {
    lubridate::hour(shift_date) <- 7
  } else if (am_or_pm == 'pm') {
    lubridate::hour(shift_date) <- 19
  }
  #measure <- "HR" # need to pass this in as an argument
  e4_data <- pull_e4_data(
    e4_ids = e4_ids,
    shift_start = shift_date,
    measure = measure,
    e4_to_part_id = split(shift_df$study_member_id, shift_df$e4_id)
    )
  print(nrow(e4_data))

  # make_sync_matrix()
  # make_sync_metrics()
  # how to return results in tibble?
  return(e4_data)
}