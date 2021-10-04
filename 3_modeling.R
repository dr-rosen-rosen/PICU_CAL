library(multilevel)
keep_vars <- c('shift_day', 'study_member_id', 'time_point', 
               'EDA_Empath', 'EDA_Driver', 'EDA_AR',
               'HR_Empath', 'HR_Driver', 'HR_AR',
               'tlx_team','tlx_individual',
               'tps_action','tps_interpersonal','tps_transition')
df_NoNA <- e4_survey_df[complete.cases(e4_survey_df[,keep_vars]),]
df_NoNA$time_point <- df_NoNA$time_point - 2
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
