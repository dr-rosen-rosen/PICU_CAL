############################################################################
############################################################################
###### Code for Generating tpd Profiles
###### 
############################################################################
############################################################################
library(tidyverse)
library(MplusAutomation)

cmb_df <- cmb_df |> ungroup() |>
  mutate(row_num = row_number()) 
tpd_measures <- c(
  'eda_eda_phasic_empath_scores_1',
  'eda_eda_phasic_driver_1',
  'eda_eda_tonic_driver_5',
  'eda_eda_tonic_empath_scores_5',
  'cardiac_ibi_empath_scores_2',
  'cardiac_ibi_driver_2'
)
df_lpa <- cmb_df |>
  select(row_num,part_id,
         all_of(tpd_measures))

df_lpa_md <- df_lpa |>
  dplyr::select(all_of(tpd_measures)) |>
  careless::mahad() |>
  cbind(df_lpa) |>
  rename(mahad = 1) |>
  filter(mahad <= 50)

df_lpa_md |>
  dplyr::select(all_of(tpd_measures)) |>
  modelsummary::datasummary_correlation()

for (var in tpd_measures) {
  f <- as.formula(paste(var, '~ part_id'))
  ICC1 <- multilevel::ICC1(aov(f,data = df_lpa_md))
  print(paste0(var,': ',ICC1))
}


##############################
########## Prep for MPLUS
##########
##############################

out_f_dir <- 'first_try'

ml_lpa_df <- df_lpa_md |>
  dplyr::select(all_of(tpd_measures), row_num) |>
  rename(
    e_p_e = eda_eda_phasic_empath_scores_1,
    e_p_d = eda_eda_phasic_driver_1,
    e_t_e = eda_eda_tonic_empath_scores_5,
    e_t_d = eda_eda_tonic_driver_5,
    c_e = cardiac_ibi_empath_scores_2,
    c_d = cardiac_ibi_driver_2
  )

df_prep <- df_lpa_md |>
  dplyr::select(all_of(tpd_measures),row_num) |>
  rename(
    e_p_e = eda_eda_phasic_empath_scores_1,
    e_p_d = eda_eda_phasic_driver_1,
    e_t_e = eda_eda_tonic_empath_scores_5,
    e_t_d = eda_eda_tonic_driver_5,
    c_e = cardiac_ibi_empath_scores_2,
    c_d = cardiac_ibi_driver_2
  ) 
prepareMplusData(df = df_prep,
                   filename = glue::glue('mplus_analyses/{out_f_dir}/profile_mPlus.dat')
  )

# formatting for MPLUS script
tpd_metrics_short <- c('e_p_e','e_p_d','e_t_e',
                       'e_t_d','c_e','c_d')

tpd_string <- paste(unlist(tpd_metrics_short), collapse = ' ')
tpd_string <- str_replace_all(tpd_string, "\n ", "\n")

#### Automating MPLUS models

class_str <- character()

ml_lpa1_10 <- lapply(1:10, function(k)
{
  
  for (x in 1:k) {
    if (x == 1) {
      class_str <<- glue::glue("%C#{x}%
[{tpd_string}];
{tpd_string};")
    } else {
      class_str <<- paste(class_str,
                          glue::glue("%C#{x}%
[{tpd_string}];
{tpd_string};"
                          ), sep="\n")
    }
    print(class_str)
  }
  
  lca_enum <- MplusAutomation::mplusObject(
    TITLE = glue::glue("L1_P_{k}_lpa_freeVar;"),
    VARIABLE = glue::glue("IDVARIABLE IS row_num;\nCLASSES = c({k});"),
    DEFINE = glue::glue("STANDARDIZE {tpd_string};"),
    ANALYSIS = "TYPE = MIXTURE;
    ESTIMATOR=MLR;
    STARTS=1000 50;
    STITERATIONS=50;
    LRTSTARTS=1000 50 1000 50;
    PROCESSORS=12;",
    MODEL = glue::glue('%OVERALL%\n',class_str),
    OUTPUT = "SAMPSTAT STANDARDIZED MOD (5.00) TECH7 TECH11 TECH13 TECH14;",
    usevariables = colnames(ml_lpa_df),
    rdata = ml_lpa_df
  )
  
  ml_lpa_fit <- MplusAutomation::mplusModeler(lca_enum,
                                              dataout = glue::glue("mplus_analyses/{out_f_dir}/L1_prof_{k}_ml_lpa_freeVar_enum.dat"),
                                              modelout = glue::glue("mplus_analyses/{out_f_dir}/L1_prof_{k}_ml_lpa_freeVar_enum.inp"),
                                              check = TRUE, run = TRUE, hashfilename = FALSE)
}
)

output_enum <- MplusAutomation::readModels(here::here(glue::glue("mplus_analyses/{out_f_dir}")), quiet = TRUE)
enum_summary <- LatexSummaryTable(output_enum,
                                  keepCols=c("Title",
                                             "LL",
                                             "BIC",
                                             "aBIC"),
                                  sortBy = "Title")
enum_summary %>%
  gt::gt()

l1_k_profiles_plot <- enum_summary %>%
  as.data.frame() %>%
  filter(str_detect(Title, pattern = 'L1_')) %>%
  # filter(str_detect(Title, pattern = 'L2', negate = TRUE)) %>%
  pivot_longer(
    !Title, names_to = 'criteria', values_to = 'value'
  ) %>%
  mutate(
    num_profiles = readr::parse_number(str_remove(Title,'L1_P_'))
  ) %>%
  filter(criteria != 'LL') %>%
  ggplot(aes(x = num_profiles, y = value, group = criteria, color = criteria)) + 
  geom_point() + geom_line() + ggthemes::geom_rangeframe() + ggthemes::theme_tufte() + 
  scale_x_continuous(breaks= scales::pretty_breaks()) + theme(legend.position = 'bottom') +
  labs(
    title='Model fit by number of daily (L1) profiles',
    x = 'Number of daily (L1) profiles',
    y = 'Model fit statistic value',
    color = 'Fit statistic')

################################
######## Check for fit for different number of L2 vars
################################

out_f_dir <- 'first_try'

ml_lpa_df <- df_lpa_md |>
  dplyr::select(all_of(tpd_measures),part_id,row_num) |>
  rename(
    e_p_e = eda_eda_phasic_empath_scores_1,
    e_p_d = eda_eda_phasic_driver_1,
    e_t_e = eda_eda_tonic_empath_scores_5,
    e_t_d = eda_eda_tonic_driver_5,
    c_e = cardiac_ibi_empath_scores_2,
    c_d = cardiac_ibi_driver_2
  )
df_prep <- df_lpa_md |>
  dplyr::select(all_of(tpd_measures),part_id,row_num) |>
  rename(
    e_p_e = eda_eda_phasic_empath_scores_1,
    e_p_d = eda_eda_phasic_driver_1,
    e_t_e = eda_eda_tonic_empath_scores_5,
    e_t_d = eda_eda_tonic_driver_5,
    c_e = cardiac_ibi_empath_scores_2,
    c_d = cardiac_ibi_driver_2
  ) 
prepareMplusData(df = df_prep,
                   filename = glue::glue('mplus_analyses/{out_f_dir}/profile_mPlus.dat')
  )

class_str <- character()

k <- 4 # L1 profiles
ml_lpa2 <- lapply(1:10, function(j) # L2 profiles
{
  
  for (x in 1:k) {
    if (x == 1) {
      class_str <<- glue::glue("%C#{x}%
[{tpd_string}];
 {tpd_string};")
    } else {
      class_str <<- paste(class_str,
                          glue::glue("%C#{x}%
[{tpd_string}];
 {tpd_string};"
                          ), sep="\n")
    }
    print(class_str)
  }
  
  lca_enum <- MplusAutomation::mplusObject(
    TITLE = glue::glue("L2_{j}_P_{k}_lpa_freeVar;"),
    VARIABLE = glue::glue("IDVARIABLE IS row_num;\nCLASSES = BC({j}) c({k});\nCLUSTER IS part_id;\nWITHIN ARE {tpd_string};\nBETWEEN ARE BC;"),
    DEFINE = glue::glue("STANDARDIZE  {tpd_string};"),
    ANALYSIS = "TYPE = MIXTURE TWOLEVEL;
    ESTIMATOR=MLR;
    STARTS=1000 200;
    STITERATIONS=50;
    LRTSTARTS=1000 50 1000 50;
    PROCESSORS=12;",
    MODEL = glue::glue('%WITHIN%\n%OVERALL%\n%BETWEEN%\n%OVERALL%\nc ON BC;\nMODEL c:\n%WITHIN%\n',class_str),
    OUTPUT = "SAMPSTAT STANDARDIZED MOD (5.00) TECH7 TECH11 TECH13 TECH14;",
    # PLOT = "type = plot3;",
    SAVEDATA = glue::glue("file=mlLpa_L2_{j}_L1_{k}.dat;\nsave=cprob;\nTECH4 IS tech4.dat;"),
    usevariables = colnames(ml_lpa_df),
    rdata = ml_lpa_df
  )
  
  ml_lpa_fit <- MplusAutomation::mplusModeler(lca_enum,
                                              dataout = glue::glue("mplus_analyses/{out_f_dir}/L2_{j}_P_{k}_ml_lpa_freeVar.dat"),
                                              modelout = glue::glue("mplus_analyses/{out_f_dir}/L2_{j}_P_{k}_ml_lpa_freeVar.inp"),
                                              check = TRUE, run = TRUE, hashfilename = FALSE)
}
)


output_enum <- MplusAutomation::readModels(here::here(glue::glue("mplus_analyses/{out_f_dir}")), quiet = TRUE)
enum_summary <- LatexSummaryTable(output_enum,
                                  keepCols=c("Title",
                                             "LL",
                                             "BIC",
                                             "aBIC"),
                                  sortBy = "Title")
enum_summary %>%
  gt::gt()
k <- 4
l2_k_range_plot <- enum_summary %>%
  as.data.frame() %>%
  filter(str_detect(Title,pattern = 'L1_', negate = TRUE)) %>%
  filter(str_detect(Title, pattern = glue::glue('_P_{k}_'))) %>%
  pivot_longer(
    !Title, names_to = 'criteria', values_to = 'value'
  ) %>%
  mutate(
    num_profiles = readr::parse_number(str_remove(Title,'L2_'))
  ) %>%
  filter(criteria != 'LL') %>%
  ggplot(aes(x = num_profiles, y = value, group = criteria, color = criteria)) + 
  geom_point() + geom_line() + ggthemes::geom_rangeframe() + ggthemes::theme_tufte() + 
  scale_x_continuous(breaks= scales::pretty_breaks()) + theme(legend.position = 'bottom') +
  labs(
    title='Model fit by number of clinician (L2) classes \nfor a 4 L1 profile model',
    x = 'Number of clinician (L2) classes',
    y = 'Model fit statistic value',
    color = 'Fit statistic')

library(patchwork)

l1_k_profiles_plot + l2_k_range_plot + plot_layout(guides = "collect")  & theme(legend.position = 'bottom') & plot_annotation(tag_levels = 'A')
# test <- MplusAutomation::getSavedata_Fileinfo(glue::glue('mplus_analyses/psych_process/L2_{L2}_P_{L1}_ml_lpa_freeVar.out')) 
k <- 2
mlLPA_results <- MplusAutomation::readModels(glue::glue("mplus_analyses/{out_f_dir}/L2_{k}_P_4_ml_lpa_freeVar.out"), what="savedata")$savedata
skimr::skim(mlLPA_results)
# L! bar charts faceted by profile
l1_by_tpd_plot <- mlLPA_results %>%
  rename('L1' = 'C',
         'L2' = 'BC') %>%
  dplyr::select(E_P_E:C_D, L1) %>%
  drop_na() %>%
  # rename('row_num' = ROW_NUM) %>%
  # select(-starts_with("V")) %>%
  pivot_longer(
    cols = E_P_E:C_D,
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    L1 = as.factor(L1)#,
    # variable = ordered(tolower(variable), levels = c("EDA_T_E", "EDA_T_D","EDA_P_E","EDA_P_D","IBI_D","IBI_E"))
  ) %>%
  group_by(L1, variable) %>%
  summarize(
    m = mean(value),
    sd = sd(value, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = variable, y = m, fill = variable)) + geom_col() + facet_grid(~L1) + 
  ggthemes::theme_tufte() + theme(
    legend.position="bottom",
    panel.background = element_rect(fill = NA, color = "black"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  ) + labs(fill = 'Linguistic dimension',
           y = 'Mean standardized value',
           x = 'Linguistic dimension',
           title = 'Linguistic dimensions by interaction (L1) profile')

# L2 profiles by L1 composition
l2_by_l1_plot <- mlLPA_results %>% 
  rename('L1' = 'C',
         'L2' = 'BC') %>%
  janitor::clean_names() %>%
  select(l1,l2) %>%
  mutate(
    l1 = as.factor(l1),
    l2 = as.factor(l2)
  ) %>%
  group_by(l2,l1) %>%
  summarize(l1_count = n())%>%
  ungroup() %>%
  group_by(l2) %>%
  mutate(l1_perc = l1_count / sum(l1_count)) %>%
  ungroup() %>%
  ggplot(aes(fill=l1, y=l1_perc, x=l2)) + 
  geom_bar(position="fill", stat="identity") + ggthemes::theme_tufte() + 
  theme(legend.position="bottom") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = 'Frequency of L1 interaction profiles in L2 clinician classes',
    x = 'L2 clinician class',
    y = 'Percentage of L1 profiles',
    fill = 'L1 profile'
  )


k <- 4
l2_k_range_plot <- enum_summary %>%
  as.data.frame() %>%
  filter(str_detect(Title,pattern = 'L1_', negate = TRUE)) %>%
  filter(str_detect(Title, pattern = glue::glue('_P_{k}_'))) %>%
  pivot_longer(
    !Title, names_to = 'criteria', values_to = 'value'
  ) %>%
  mutate(
    num_profiles = readr::parse_number(str_remove(Title,'L2_'))
  ) %>%
  filter(criteria != 'LL') %>%
  ggplot(aes(x = num_profiles, y = value, group = criteria, color = criteria)) + 
  geom_point() + geom_line() + ggthemes::geom_rangeframe() + ggthemes::theme_tufte() + 
  scale_x_continuous(breaks= scales::pretty_breaks()) + theme(legend.position = 'bottom') +
  labs(
    title='Model fit by number of clinician (L2) classes \nfor a 4 L1 profile model',
    x = 'Number of clinician (L2) classes',
    y = 'Model fit statistic value',
    color = 'Fit statistic')

library(patchwork)

l1_k_profiles_plot + l2_k_range_plot + plot_layout(guides = "collect")  & theme(legend.position = 'bottom') & plot_annotation(tag_levels = 'A')
# test <- MplusAutomation::getSavedata_Fileinfo(glue::glue('mplus_analyses/psych_process/L2_{L2}_P_{L1}_ml_lpa_freeVar.out')) 
k <- 2
mlLPA_results <- MplusAutomation::readModels(glue::glue("mplus_analyses/{out_f_dir}/L2_{k}_P_4_ml_lpa_freeVar.out"), what="savedata")$savedata
skimr::skim(mlLPA_results)
# L! bar charts faceted by profile
l1_by_tpd_plot <- mlLPA_results %>%
  rename('L1' = 'C',
         'L2' = 'BC') %>%
  dplyr::select(E_P_E:C_D, L1) %>%
  drop_na() %>%
  # rename('row_num' = ROW_NUM) %>%
  # select(-starts_with("V")) %>%
  pivot_longer(
    cols = E_P_E:C_D,
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    L1 = as.factor(L1)#,
    # variable = ordered(tolower(variable), levels = c("EDA_T_E", "EDA_T_D","EDA_P_E","EDA_P_D","IBI_D","IBI_E"))
  ) %>%
  group_by(L1, variable) %>%
  summarize(
    m = mean(value),
    sd = sd(value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    sync_meas_group = case_when(
      str_detect(variable,'C_') ~ 'Cardiac',
      str_detect(variable,'E_T_') ~ 'EDA Tonic',
      str_detect(variable,'E_P_') ~ 'EDA Phasic'
    )
  ) |>
  ggplot(aes(x = variable, y = m, fill = sync_meas_group)) + geom_col() + facet_grid(~L1) + 
  ggthemes::theme_tufte() + theme(
    legend.position="bottom",
    panel.background = element_rect(fill = NA, color = "black"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  ) + labs(fill = 'TPD measure type',
           y = 'Mean standardized value',
           x = 'Synchrony measure',
           title = 'Team Physiological Dynamics (TPD) by L1 profile') +
  viridis::scale_color_viridis(discrete = TRUE, option = 'D') +
  viridis::scale_fill_viridis(discrete = TRUE)

# L2 profiles by L1 composition
l2_by_l1_plot <- mlLPA_results %>% 
  rename('L1' = 'C',
         'L2' = 'BC') %>%
  janitor::clean_names() %>%
  select(l1,l2) %>%
  mutate(
    l1 = as.factor(l1),
    l2 = as.factor(l2)
  ) %>%
  group_by(l2,l1) %>%
  summarize(l1_count = n())%>%
  ungroup() %>%
  group_by(l2) %>%
  mutate(l1_perc = l1_count / sum(l1_count)) %>%
  ungroup() %>%
  ggplot(aes(fill=l1, y=l1_perc, x=l2)) + 
  geom_bar(position="fill", stat="identity") + ggthemes::theme_tufte() + 
  theme(legend.position="bottom") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = 'Frequency of L1 TPD profiles in L2 team member classes',
    x = 'L2 team member class',
    y = 'Percentage of L1 profiles',
    fill = 'L1 profile'
  ) 

mlLPA_results |>
  select(BC, PART_ID) |>
  distinct() |>
  group_by(BC) |>
  summarize(n = n())

mlLPA_results |>
  # select(C, PART_ID) |>
  # distinct() |>
  group_by(C) |>
  summarize(n = n())

###############################
###### Structure data for LG
###############################


L1 <- 4
mergedResults2 <- mlLPA_results |>
  mutate(
    #create composite probabilities for a 4 by 2
    L2_1_prob = rowSums(pick(num_range("CPROB",1:4))),
    L2_2_prob = rowSums(pick(num_range("CPROB",5:8))),
    L1_1_prob = rowSums(pick(num_range("CPROB",c(1,5)))),
    L1_2_prob = rowSums(pick(num_range("CPROB",c(2,6)))),
    L1_3_prob = rowSums(pick(num_range("CPROB",c(3,7)))),
    L1_4_prob = rowSums(pick(num_range("CPROB",c(4,8))))
    
    # create composite probabilities for 4 by 3
    # L2_1_prob = rowSums(pick(num_range("CPROB",1:4))),
    # L2_2_prob = rowSums(pick(num_range("CPROB",5:8))),
    # L2_3_prob = rowSums(pick(num_range("CPROB",9:12))),
    # L1_1_prob = rowSums(pick(num_range("CPROB",c(1,5,9)))),
    # L1_2_prob = rowSums(pick(num_range("CPROB",c(2,6,10)))),
    # L1_3_prob = rowSums(pick(num_range("CPROB",c(3,7,11)))),
    # L1_4_prob = rowSums(pick(num_range("CPROB",c(4,8,12))))
  ) |>
  select(BC,C,ROW_NUM,PART_ID,starts_with("L")) |>
  rename("M{L1}_L1" := 'C',
         "M{L1}_L2" := 'BC') |>
  janitor::clean_names() |> select(-part_id) |>
  left_join(cmb_df, by = c('row_num')) 


skimr::skim(mergedResults)
haven::write_sav(mergedResults2,'picu_for_lg_2by4_06-09-2024.sav')

# key |> write.csv('good_data.csv')
# mergedResults2 |>
#   select(part_id,m2_l2,id_crew,campaign,mission) |>
#   distinct() |> write.csv('messed_up_data.csv')
# mlLPA_results |> select(BC,PART_ID) |>
#   distinct()