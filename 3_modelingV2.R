####################################################################################################
####################################################################################################
################################ V2 Modeling go
####################################################################################################
####################################################################################################

library(lme4)
library(sjPlot)
# library(multilevel)
keep_vars <- c('shift_day', 'study_member_id', 'time_point', 
               'stressors_total', 'stressors_team','stressors_interpersonal','stressors_ind',
               'stressors_total_dich', 'stressors_team_dich','stressors_interpersonal_dich','stressors_ind_dich',
               # 'EDA_Empath', 'EDA_Driver', 'EDA_AR',
               # 'HR_Empath', 'HR_Driver', 'HR_AR',
               'tlx_team','tlx_individual',
               'tps_action','tps_interpersonal','tps_transition',
               'mbi','belief_process','belief_potency',
               'trust_prop_t','trust_perc_t','trust_coop_beh','trust_mon_beh'
               )

df_NoNA <- survey_data[,keep_vars] %>%
  group_by(shift_day,study_member_id) %>%
  fill(mbi, .direction = "up") %>%
  fill(belief_process, .direction = "up") %>%
  fill(belief_potency, .direction = "up") %>%
  fill(trust_prop_t, .direction = 'up') %>%
  fill(trust_perc_t, .direction = 'up') %>%
  fill(trust_coop_beh, .direction = 'up') %>%
  fill(trust_mon_beh, .direction = 'up') %>%
  ungroup() %>%
  drop_na()
df_NoNA$time_point <- df_NoNA$time_point - 2

# df_NoNA <- survey_data[complete.cases(survey_data[,keep_vars]),]
# df_NoNA$time_point <- df_NoNA$time_point - 2
skimr::skim(df_NoNA)




M.null <- lmer(tlx_team ~ 1 + (1|study_member_id), data = df_NoNA)
M.null.2 <- lmer(tlx_team ~ 1 + (1|time_point) + (1|study_member_id) + (1|shift_day), data = df_NoNA)
anova(M.null,M.null.2)
summary(M.null.2)

test_1 <- lmer(tlx_individual ~ 1 + 
                 stressors_total +
                 # stressors_team +
                 # stressors_ind +
                 # stressors_interpersonal +
                 # stressors_total_dich + 
                 # stressors_team_dich +
                 # stressors_ind_dich +
                 # stressors_interpersonal_dich +
                 tps_action + 
                 tps_interpersonal + 
                 tps_transition + 
                 stressors_total:tps_action +
                 tps_interpersonal:stressors_interpersonal +
                 stressors_total:tps_transition +
                 (1|time_point) + 
                 (1|study_member_id) + 
                 (1|shift_day), 
               data = df_NoNA)
# anova(M.null.2,test_1)
# summary(test_1)
sjPlot::tab_model(test_1)
sjPlot::plot_model(test_1, type = 'int')
hist(df_NoNA$stressors_total)



M.MBI.1 <- lmer(mbi ~ 1 + 
                  # stressors_total +
                  tlx_team +
                  tlx_individual +
                  # stressors_team +
                  # stressors_ind +
                  # stressors_interpersonal +
                  # stressors_total_dich +
                  # stressors_team_dich +
                  # stressors_ind_dich +
                  # stressors_interpersonal_dich +
                  tps_action + 
                  tps_interpersonal + 
                  tps_transition + 
                  # stressors_total:tps_action +
                  # stressors_total:tps_interpersonal +
                  # stressors_total:tps_transition +
                  tlx_individual:tps_action +
                  tlx_individual:tps_interpersonal +
                  tlx_individual:tps_transition +
                  # tlx_team:tps_action +
                  # tlx_team:tps_interpersonal +
                  # tlx_team:tps_transition +
                  (1|study_member_id) + (1|shift_day), data = df_NoNA)
sjPlot::tab_model(M.MBI.1)
sjPlot::plot_model(M.MBI.1, type = 'int')
hist(df_NoNA$mbi)

M.belief.1 <- lmer(belief_process ~ 1 + 
                     stressors_total +
                  # stressors_team +
                  # stressors_ind +
                  # stressors_interpersonal +
                  # stressors_total_dich +
                  # stressors_team_dich +
                  # stressors_ind_dich +
                  # stressors_interpersonal_dich +
                  tps_action + 
                  tps_interpersonal + 
                  tps_transition + 
                  # stressors_total:tps_action +
                  # stressors_total:tps_interpersonal +
                  # stressors_total:tps_transition +
                  (1|study_member_id) + (1|shift_day), data = df_NoNA)
sjPlot::tab_model(M.belief.1)
sjPlot::plot_model(M.belief.1, type = 'int')
hist(df_NoNA$belief_process)

M.potency.1 <- lmer(belief_potency ~ 1 + 
                      # stressors_total +
                     # stressors_team +
                     # stressors_ind +
                     # stressors_interpersonal +
                     # stressors_total_dich + 
                     stressors_team_dich +
                     stressors_ind_dich +
                     stressors_interpersonal_dich +
                     tps_action + 
                     tps_interpersonal + 
                     tps_transition + 
                     # stressors_total:tps_action +
                     # stressors_total:tps_interpersonal +
                     # stressors_total:tps_transition +
                     (1|study_member_id) + (1|shift_day), data = df_NoNA)
sjPlot::tab_model(M.potency.1)
sjPlot::plot_model(M.potency.1, type = 'int')
hist(df_NoNA$belief_potency)


M.trust.1 <- lmer(trust_perc_t ~ 1 + 
                      stressors_total +
                      tlx_team +
                      tlx_individual +
                      # stressors_team +
                      # stressors_ind +
                      # stressors_interpersonal +
                      # stressors_total_dich + 
                      # stressors_team_dich +
                      # stressors_ind_dich +
                      # stressors_interpersonal_dich +
                      tps_action + 
                      tps_interpersonal + 
                      tps_transition + 
                      # stressors_total:tps_action +
                      # stressors_total:tps_interpersonal +
                      # stressors_total:tps_transition +
                      # tlx_individual:tps_action +
                      # tlx_individual:tps_interpersonal +
                      # tlx_individual:tps_transition +
                      tlx_team:tps_action +
                      tlx_team:tps_interpersonal +
                      tlx_team:tps_transition +
                      (1|study_member_id) + (1|shift_day), data = df_NoNA)
sjPlot::tab_model(M.trust.1)
sjPlot::plot_model(M.trust.1, type = 'int')
hist(df_NoNA$trust_prop_t)


vars_to_scale <- c('EDA_Empath', 'EDA_Driver', 'EDA_AR',
                   'HR_Empath', 'HR_Driver', 'HR_AR',
                   'tlx_team','tlx_individual',
                   'tps_action','tps_interpersonal','tps_transition')
for (var in vars_to_scale) {
  df_NoNA[[var]] <- scale(df_NoNA[[var]], center = TRUE, scale = TRUE)
}

#Null model with grouping structure
Null.Model <- lme(tlx_team ~ 1, random = ~1|study_member_id, data = df_NoNA, method = 'ML', control = list(opt='optim'),na.action=na.omit)
VarCorr(Null.Model)
summary(Null.Model)

#Model with no grouping variable
Null.Model.2 <- gls(tlx_team ~ 1, data = df_NoNA, method = 'ML', control = list(opt='optim'),na.action=na.omit)
#Compare the two null models
anova(Null.Model,Null.Model.2)
-2*logLik(Null.Model)
-2*logLik(Null.Model.2)

Null.Model.Time <- lme(tlx_team ~ time_point, random = ~1|study_member_id, data = df_NoNA, method = 'ML', control = list(opt='optim'),na.action=na.omit)
VarCorr(Null.Model.Time)
summary(Null.Model.Time)
anova(Null.Model,Null.Model.Time)
-2*logLik(Null.Model.Time)


# add empath
Null.Model.Time2 <- lme(tlx_team ~ time_point + EDA_Empath, random = ~1|study_member_id, data = df_NoNA, method = 'ML', control = list(opt='optim'),na.action=na.omit)
VarCorr(Null.Model.Time2)
summary(Null.Model.Time2)
anova(Null.Model.Time,Null.Model.Time2)
-2*logLik(Null.Model.Time2)


library(lme4)
# M.null.test <- lmer(tlx_team ~ 1 + (1|shift_day), df_NoNA)
# M.null.test.2 <- lmer(tlx_team ~ 1 + (1|study_member_id) + (1|shift_day), df_NoNA)
# anova(M.null.test,M.null.test.2)
M.null <- lmer(tlx_individual ~ 1 + (1|study_member_id) + (1|shift_day), df_NoNA)
M.time <- lmer(tlx_individual ~ time_point + (1|study_member_id) + (1|shift_day), df_NoNA)
anova(M.null,M.time)
M.time.empath <- lmer(tlx_team ~ poly(time_point,2) + 
                        tps_action + tps_interpersonal + tps_transition + 
                        EDA_Driver + EDA_AR + EDA_Empath + 
                        HR_Driver + HR_AR + HR_Empath +
                        (1|study_member_id) + (1|shift_day), df_NoNA)
anova(M.time,M.time.empath)



library(sjPlot)
library(lmerTest)
sjPlot::plot_model(M.time.empath, type = 'std')
summary(M.time.empath)
anova(M.time.empath)
summary(M.time.empath, ddf="Kenward-Roger")
VarCorr(M.time.empath)
