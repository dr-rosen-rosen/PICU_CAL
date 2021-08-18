###############################################################################################
###############################################################################################
#####################   FUNCS for Processing and Analyzing PICU CAL data
#####################
###############################################################################################
###############################################################################################


library(readr)
library(tidyverse)

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
                                   stressors_13)))
  
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
  # picu_data <-picu_data %>%
  #   rowwise() %>%
  #   mutate(mayo_9 = recode(mayo_9, "N/A" = "1")) 
  # picu_data <- picu_data %>%
  #   rowwise() %>%
  #   mutate(mayo_10 = recode(mayo_10, "N/A" = "1")) 
  # picu_data <- picu_data %>%
  #   rowwise() %>%
  #   mutate(mayo_11 = recode(mayo_11, "N/A" = "1")) 
  # picu_data <- picu_data %>%
  #   rowwise() %>%
  #   mutate(mayo_12 = recode(mayo_12, "N/A" = "1")) 
  # picu_data <- picu_data %>%
  #   rowwise() %>%
  #   mutate(mayo_13 = recode(mayo_13, "N/A" = "1")) 
  # picu_data <- picu_data %>%
  #   rowwise() %>%
  #   mutate(mayo_14 = recode(mayo_14, "N/A" = "1")) 
  # picu_data <- picu_data %>%
  #   rowwise() %>%
  #   mutate(mayo_15 = recode(mayo_15, "N/A" = "1")) 
  # picu_data <- picu_data %>%
  #   rowwise() %>%
  #   mutate(mayo_16 = recode(mayo_16, "N/A" = "1")) 
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

