############################################################################
############################################################################
###### Code for Generating tpd Profiles
###### 
############################################################################
############################################################################
library(tidyverse)

cmb_df <- cmb_df |>
  mutate(row_num = row_number()) |>
  rename(part_id = study_member_id)
tpd_measures <- c(
  'eda_eda_phasic_empath_scores_1',
  'eda_eda_phasic_driver_1',
  'eda_eda_tonic_empath_scores_5',
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
  dplyr::select(all_of(tpd_measures), row_num, part_id) |>
  rename(
    e_p_e = eda_eda_phasic_empath_scores_1,
    e_p_d = eda_eda_phasic_driver_1,
    e_t_e = eda_eda_tonic_empath_scores_5,
    e_t_d = eda_eda_tonic_empath_scores_5,
    c_e = cardiac_ibi_empath_scores_2,
    c_d = cardiac_ibi_driver_2
  )

df_prep <- df

