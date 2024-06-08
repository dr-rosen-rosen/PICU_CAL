library(MplusAutomation)
library(tidySEM)
keep_vars <- c('shift_day', 'study_member_id', 'time_point', 
               # admin stressors
               # 'team_ppv', 'team_census', 'team_vaso','team_ecmo','team_renal',
               # 'team_cardiac',
               'code_event',
               'picu_census','admit_picu','transfer_picu','discharge_picu',
               'beside_nurse_picu',
               # self-report stressors
               'stressors_total','stressors_team','stressors_interpersonal','stressors_ind',
               # mbi items
               'mbi','mbi_1','mbi_2',
               #tlx individual
               'tlx_1','tlx_2','tlx_3','tlx_4','tlx_5','tlx_6',
               #tlx team
               'tlx_7','tlx_8','tlx_9','tlx_10','tlx_11','tlx_12',
               # tps action
               'tps_4', 'tps_5', 'tps_6', 'tps_7',
               # tps interpersonal
               'tps_8', 'tps_9', 'tps_10',
               # tps transition
               'tps_1', 'tps_2', 'tps_3',
               # tps composite
               
               # trust sub scales
               'trust_prop_t','trust_perc_t','trust_coop_beh','trust_mon_beh',
               # efficacy
               'belief_process','belief_potency',
               
               # individual physio
               # 'hr_mean', 'eda_mean',
               
               'mean_eda_tonic','mean_eda_phasic','scr_peak_count',
                'rmssd','bpm','ibi',
               
               
               # team physio
               # "hr_empath_scores_60", "eda_empath_scores_60",
               # "hr_driver_60", "eda_driver_60",
               # "eda_s_e_60","hr_s_e_60"
               'cardiac_rmssd_empath_scores_2','cardiac_rmssd_driver_2',
               'cardiac_rmssd_s_e_2',
               'eda_eda_tonic_s_e_2', 'eda_eda_phasic_s_e_2'
               
               
)

shift_level_vars <- c(
  'mbi','mbi_1','mbi_2','trust_prop_t','trust_perc_t','trust_coop_beh','trust_mon_beh',
  'belief_process','belief_potency'
  )
shift_level_vars2 <- c(#'team_ppv', 'team_census', 'team_ppv', 'team_vaso','team_ecmo','team_renal',
                       #'team_cardiac',
                       'picu_census','admit_picu','transfer_picu','discharge_picu',
                       'beside_nurse_picu')

skimr::skim(cmb_df[keep_vars])

df_MPLUS <- cmb_df[,keep_vars] %>%
  group_by(shift_day,study_member_id) %>%
  fill(!!shift_level_vars, .direction = "up") %>%
  ungroup() %>%
  group_by(shift_day) %>%
  fill(!!shift_level_vars2, .direction = "downup") %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
    churn = admit_picu + transfer_picu + discharge_picu,
    pt_rn_rat = beside_nurse_picu/picu_census
  )
  # drop_na()
# df_MPLUS$time_point <- df_MPLUS$time_point - 2
df_MPLUS <- df_MPLUS %>%
  # mutate(across(!shift_day & !study_member_id & !time_point, scale)) %>%
  mutate(
    shift = factor(shift_day, ordered = FALSE),
    part_id = factor(study_member_id, ordered = FALSE)
  ) %>%
  select(-c(shift_day, study_member_id))
skimr::skim(df_MPLUS)
# df_MPLUS <- df_MPLUS %>% select(-c(mbi_1,mbi_2, shift, part_id))

MplusAutomation::prepareMplusData(
  df_MPLUS,
  filename = here('mplus_analyses','PICU_mplus.dat')#,
  # dropCols = c('shift_day','study_member_id')
  # dummyCode = c('shift_day','study_member_id')
)

sem_df <- df_MPLUS |> select(shift,stressors_total,churn,pt_rn_rat,code_event,
                             tlx_1,tlx_2,tlx_3,tlx_4,tlx_5,tlx_6 ,
                             tlx_7,tlx_8,tlx_9,tlx_10,tlx_11,tlx_12,
                             mbi,
                             trust_prop_t,trust_perc_t,trust_coop_beh,trust_mon_beh,
                             belief_process,belief_potency) |>
  rename(blf_pr = belief_process, blf_po = belief_potency)


m.0 <- MplusAutomation::mplusObject(
  TITLE = "PICU CAL, base model;",
  # DEFINE = "STANDARDIZE ;",
  MISSING = '.;',
  VARIABLE = "Cluster = shift;",
  ANALYSIS = "TYPE = TWOLEVEL; ESTIMATOR = BAYES; PROCESSORS = 6;",
  MODEL = paste("%WITHIN%",
                #"ind_wkld by tlx_1 tlx_2 tlx_3 tlx_4 tlx_5 tlx_6;",
                #"tm_wkld by tlx_7 tlx_8 tlx_9 tlx_10 tlx_11 tlx_12;", 
                "trust by trust_prop_t trust_perc_t trust_coop_beh trust_mon_beh;",
                "eff by blf_pr blf_po;",
                "mbi trust eff on stressors_total churn pt_rn_rat code_event;",
                "%BETWEEN%",
                #"ind_wkld_b by tlx_1 tlx_2 tlx_3 tlx_4 tlx_5 tlx_6;",
                #"tm_wkld_b by tlx_7 tlx_8 tlx_9 tlx_10 tlx_11 tlx_12;",
                "trust_b by trust_prop_t trust_perc_t trust_coop_beh trust_mon_beh;",
                "eff_b by blf_pr blf_po;",
                #"ind_wkld_b on churn pt_rn_rat stressors_total;",
                #"tm_wkld_b on churn pt_rn_rat stressors_total;",
                #"mbi on ind_wkld_b tm_wkld_b;",
                "mbi trust_b eff_b on stressors_total churn pt_rn_rat code_event;",
                #"trust_b on ind_wkld_b tm_wkld_b;",
                #"eff_b on ind_wkld_b tm_wkld_b;",
                sep = "\n"),
  usevariables = colnames(sem_df),
  OUTPUT = 'standardized;',
  rdata = sem_df
)

m.0r <- MplusAutomation::mplusModeler(m.0,
                                      modelout = here::here('mplus_auto','m.0.inp'),
                                      dataout = here::here('mplus_auto','m0.dat'),
                                      run = TRUE,
                                      check = TRUE,
                                      hashfilename = FALSE)



m.1 <- MplusAutomation::mplusObject(
  TITLE = "PICU CAL, base plus workload model;",
  # DEFINE = "STANDARDIZE ;",
  MISSING = '.;',
  VARIABLE = "Cluster = shift;",
  ANALYSIS = "TYPE = TWOLEVEL; ESTIMATOR = BAYES; PROCESSORS = 6;",
  MODEL = paste("%WITHIN%",
                "ind_wkld by tlx_1 tlx_2 tlx_3 tlx_4 tlx_5 tlx_6;",
                #"tm_wkld by tlx_7 tlx_8 tlx_9 tlx_10 tlx_11 tlx_12;", 
                #"trust by trust_prop_t trust_perc_t trust_coop_beh trust_mon_beh;",
                #"eff by blf_pr blf_po;",
                "ind_wkld on stressors_total code_event;",
                "mbi on ind_wkld",
                "%BETWEEN%",
                #"ind_wkld_b by tlx_1 tlx_2 tlx_3 tlx_4 tlx_5 tlx_6;",
                "tm_wkld_b by tlx_7 tlx_8 tlx_9 tlx_10 tlx_11 tlx_12;",
                "trust_b by trust_prop_t trust_perc_t trust_coop_beh trust_mon_beh;",
                "eff_b by blf_pr blf_po;",
                #"ind_wkld_b on churn pt_rn_rat stressors_total;",
                #"tm_wkld_b on churn pt_rn_rat stressors_total;",
                #"mbi on ind_wkld_b tm_wkld_b;",
                #"ind_wkld_b tm_wkld_b on stressors_total churn pt_rn_rat code_event;",
                "tm_wkld_b on stressors_total churn pt_rn_rat code_event;",
                "mbi trust_b eff_b on tm_wkld_b;",
                #"mbi trust_b eff_b on ind_wkld_b tm_wkld_b;",
                #"trust_b on ind_wkld_b tm_wkld_b;",
                #"eff_b on ind_wkld_b tm_wkld_b;",
                sep = "\n"),
  usevariables = colnames(sem_df),
  OUTPUT = 'standardized;',
  rdata = sem_df
)

m.1r <- MplusAutomation::mplusModeler(m.1,
                                      modelout = here::here('mplus_auto','m.1.inp'),
                                      dataout = here::here('mplus_auto','m1.dat'),
                                      run = TRUE,
                                      check = TRUE,
                                      hashfilename = FALSE)
# tidySEM::graph_sem(m.1r)


sem_df <- df_MPLUS |> select(shift,stressors_total,churn,pt_rn_rat, code_event,
                             tlx_1,tlx_2,tlx_3,tlx_4,tlx_5,tlx_6 ,
                             tlx_7,tlx_8,tlx_9,tlx_10,tlx_11,tlx_12,
                             tps_1,tps_2,tps_3,tps_4,tps_5,tps_6,tps_7,tps_8,tps_9,tps_10,
                             mbi,
                             trust_prop_t,trust_perc_t,trust_coop_beh,trust_mon_beh,
                             belief_process,belief_potency) |>
  rename(blf_pr = belief_process, blf_po = belief_potency)
m.2 <- MplusAutomation::mplusObject(
  TITLE = "PICU CAL, base model;",
  # DEFINE = "STANDARDIZE ;",
  MISSING = '.;',
  VARIABLE = "Cluster = shift;",
  ANALYSIS = "TYPE = TWOLEVEL; ESTIMATOR = BAYES; BITERATIONS = (4000); PROCESSORS = 6;",
  MODEL = paste("%WITHIN%",
                "ind_wkld by tlx_1 tlx_2 tlx_3 tlx_4 tlx_5 tlx_6;",
                #"tm_wkld by tlx_7 tlx_8 tlx_9 tlx_10 tlx_11 tlx_12;", 
                #"trust by trust_prop_t trust_perc_t trust_coop_beh trust_mon_beh;",
                #"eff by blf_pr blf_po;",
                "ind_wkld on stressors_total code_event;",
                "mbi on ind_wkld",
                "%BETWEEN%",
                #"ind_wkld_b by tlx_1 tlx_2 tlx_3 tlx_4 tlx_5 tlx_6;",
                "tm_wkld_b by tlx_7 tlx_8 tlx_9 tlx_10 tlx_11 tlx_12;",
                "trust_b by trust_prop_t trust_perc_t trust_coop_beh trust_mon_beh;",
                "eff_b by blf_pr blf_po;",
                "tm_act by tps_4 tps_5 tps_6 tps_7;",
                "tm_trs by tps_1 tps_2 tps_3;",
                "tm_int by tps_8 tps_9 tps_10;",
                #"ind_wkld_b on churn pt_rn_rat stressors_total;",
                #"tm_wkld_b on churn pt_rn_rat stressors_total;",
                #"mbi on ind_wkld_b tm_wkld_b;",
                #"ind_wkld_b tm_wkld_b on stressors_total churn pt_rn_rat code_event;",
                "tm_wkld_b on stressors_total churn pt_rn_rat code_event;",
                "tm_act tm_trs tm_int on tm_wkld_b;",
                "mbi trust_b eff_b on tm_act tm_trs tm_int;",
                #"mbi trust_b eff_b on tm_wkld_b;",
                #"mbi trust_b eff_b on ind_wkld_b tm_wkld_b;",
                #"trust_b on ind_wkld_b tm_wkld_b;",
                #"eff_b on ind_wkld_b tm_wkld_b;",
                sep = "\n"),
  usevariables = colnames(sem_df),
  OUTPUT = 'standardized;',
  rdata = sem_df
)

m.2r <- MplusAutomation::mplusModeler(m.2,
                                      modelout = here::here('mplus_auto','m.2.inp'),
                                      dataout = here::here('mplus_auto','m2.dat'),
                                      run = TRUE,
                                      check = TRUE,
                                      hashfilename = FALSE)
# tidySEM::graph_sem(m.1r)



sem_df <- df_MPLUS |> select(shift,stressors_total,churn,pt_rn_rat, code_event,
                             tlx_1,tlx_2,tlx_3,tlx_4,tlx_5,tlx_6 ,
                             tlx_7,tlx_8,tlx_9,tlx_10,tlx_11,tlx_12,
                             tps_1,tps_2,tps_3,tps_4,tps_5,tps_6,tps_7,tps_8,tps_9,tps_10,
                             mbi,
                             trust_prop_t,trust_perc_t,trust_coop_beh,trust_mon_beh,
                             belief_process,belief_potency,
                             mean_eda_phasic, mean_eda_tonic,rmssd,
                             cardiac_rmssd_s_e_2, eda_eda_tonic_s_e_2, eda_eda_phasic_s_e_2
                             ) |>
  rename(blf_pr = belief_process, blf_po = belief_potency,
         phasic = mean_eda_phasic, tonic = mean_eda_tonic, rmmssd_se = cardiac_rmssd_s_e_2,
         tonic_se = eda_eda_tonic_s_e_2, phas_se = eda_eda_phasic_s_e_2)
m.3 <- MplusAutomation::mplusObject(
  TITLE = "PICU CAL, base model;",
  # DEFINE = "STANDARDIZE ;",
  MISSING = '.;',
  VARIABLE = "Cluster = shift;",
  ANALYSIS = "TYPE = TWOLEVEL; ESTIMATOR = BAYES; BITERATIONS = (4000); PROCESSORS = 6;",
  MODEL = paste("%WITHIN%",
                "ind_wkld by tlx_1 tlx_2 tlx_3 tlx_4 tlx_5 tlx_6;",
                #"tm_wkld by tlx_7 tlx_8 tlx_9 tlx_10 tlx_11 tlx_12;", 
                #"trust by trust_prop_t trust_perc_t trust_coop_beh trust_mon_beh;",
                #"eff by blf_pr blf_po;",
                #"tm_act by tps_4 tps_5 tps_6 tps_7;",
                #"tm_trs by tps_1 tps_2 tps_3;",
                #"tm_int by tps_8 tps_9 tps_10;",
                "ind_wkld on rmssd phasic tonic;",
                #"tm_act tm_trs tm_int on ind_wkld;",
                #"mbi on tm_act tm_trs tm_int;",
                "%BETWEEN%",
                #"ind_wkld_b by tlx_1 tlx_2 tlx_3 tlx_4 tlx_5 tlx_6;",
                #"tm_wkld_b by tlx_7 tlx_8 tlx_9 tlx_10 tlx_11 tlx_12;",
                #"trust_b by trust_prop_t trust_perc_t trust_coop_beh trust_mon_beh;",
                #"eff_b by blf_pr blf_po;",
                #"tm_act_b by tps_4 tps_5 tps_6 tps_7;",
                #"tm_trs_b by tps_1 tps_2 tps_3;",
                #"tm_int_b by tps_8 tps_9 tps_10;",
                #"ind_wkld_b on churn pt_rn_rat stressors_total;",
                #"tm_wkld_b on churn pt_rn_rat stressors_total;",
                #"mbi on ind_wkld_b tm_wkld_b;",
                #"ind_wkld_b tm_wkld_b on stressors_total churn pt_rn_rat code_event;",
                #"tm_wkld_b tm_act_b tm_trs_b tm_int_b on rmmssd_se phas_se tonic_se;",
                #"tm_act_b tm_trs_b tm_int_b on tm_wkld_b;",
                #"mbi trust_b eff_b on tm_act_b tm_trs_b tm_int_b;",
                #"mbi trust_b eff_b on tm_wkld_b;",
                #"mbi trust_b eff_b on ind_wkld_b tm_wkld_b;",
                #"trust_b on ind_wkld_b tm_wkld_b;",
                #"eff_b on ind_wkld_b tm_wkld_b;",
                sep = "\n"),
  usevariables = colnames(sem_df),
  OUTPUT = 'standardized;',
  rdata = sem_df
)

m.3r <- MplusAutomation::mplusModeler(m.3,
                                      modelout = here::here('mplus_auto','m.3.inp'),
                                      dataout = here::here('mplus_auto','m3.dat'),
                                      run = TRUE,
                                      check = TRUE,
                                      hashfilename = FALSE)
# tidySEM::graph_sem(m.1r)
