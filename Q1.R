####################################################################################################
####################################################################################################
################################ Q1 Analyses -- Does Synchrony Predict things we care about?
####################################################################################################
####################################################################################################
library(tidyverse)
library(lme4)
library(gamlss)
names(e4_survey_df)
df <- e4_survey_df %>%
  filter(time_point > 1) %>% # drops time points with single stress items
  # filter((HR_prop_missing < .2) & (EDA_prop_missing < .2)) %>%
  mutate(time_point = time_point - 2) %>% # sets first full survey of shift to time point 0
  mutate(time_point = as.factor(time_point)) %>%
  mutate(shift_day = as.factor(shift_day)) %>%
  mutate(study_member_id = as.factor(study_member_id)) %>%
  dplyr::select(time_point, shift_day, study_member_id,
                HR_Empath, HR_Driver, EDA_Empath, EDA_Driver,
                EDA_Se, HR_Se,
                tlx_individual, tlx_team, 
                tps_transition, tps_action, tps_interpersonal,
                stressors_total#,
                # mayo_total
                # trust_coop_beh, trust_mon_beh,
                # belief_process, belief_potency,
                # mbi, mbi_1, mbi_2
                ) %>%
  drop_na() %>%
  # mutate(across(where(is.numeric), ~ as.numeric(base::scale(.,center = TRUE, scale = TRUE)))) %>%
  mutate(time_point = as.numeric(time_point))

WL_i.m.lmer <- lmer(tlx_individual ~ 
              time_point +
              stressors_total +
                # EDA_Empath +
                EDA_Driver +
              HR_Empath +
              # HR_Driver +
              # EDA_Se +
              # HR_Se +
              (1|study_member_id) +
              (1|shift_day),
            data = df
              )
sjPlot::tab_model(WL_i.m.lmer)

WL_t.m.lmer <- lmer(tlx_team ~ 
                      time_point +
                      stressors_total +
                      EDA_Empath +
                      # EDA_Driver +
                      HR_Empath +
                      # HR_Driver +
                      # EDA_Se +
                      # HR_Se +
                      (1|study_member_id) +
                      (1|shift_day),
                    data = df
)
sjPlot::tab_model(WL_t.m.lmer)

WLi.fit <- gamlss::fitDist(df$tlx_individual, type = 'realAll')
WLi.fit$fits
WLi.hist <- gamlss::histDist(tlx_individual, family = SEP1, nbins = 30, data = df)

WL_i.m.gamlss <- gamlss::gamlss(
  formula = tlx_individual ~ time_point +
    stressors_total + 
    EDA_Empath + 
    # EDA_Driver +
    HR_Empath +
    # HR_Driver +
    # EDA_Se +
    # HR_Se +
    re(random = ~1|study_member_id) +
    re(random = ~1|shift_day),
  family = SEP1(),
  data = df,trace = FALSE,
  control = gamlss.control(n.cyc = 2000)
)
## Evaluate
summary(WL_t.m.gamlss)
Rsq(WL_t.m.gamlss)
plot(WL_t.m.gamlss)
wp(WL_t.m.gamlss)


#### WL Team GAMLSS
WLt.fit <- gamlss::fitDist(df$tlx_team, type = 'realAll')
WLt.fit$fits
WLt.hist <- gamlss::histDist(tlx_team, family = PE, nbins = 30, data = df)

WL_t.m.gamlss <- gamlss::gamlss(
  formula = tlx_team ~ time_point +
    stressors_total + 
    EDA_Empath + 
    # EDA_Driver +
    HR_Empath +
    # HR_Driver +
    # EDA_Se +
    # HR_Se +
    re(random = ~1|study_member_id) +
    re(random = ~1|shift_day),
  family = PE(),
  data = df,trace = FALSE,
  control = gamlss.control(n.cyc = 2000)
)
## Evaluate
summary(WL_t.m.gamlss)
Rsq(WL_t.m.gamlss)
plot(WL_t.m.gamlss)
wp(WL_t.m.gamlss)

sjPlot::tab_model(WL_t.m.lmer)

TW_t.lmer <- lmer(tps_transition ~ 
                      time_point +
                      stressors_total +
                      EDA_Empath +
                      # EDA_Driver +
                      HR_Empath +
                      # HR_Driver +
                      EDA_Se +
                      HR_Se +
                      (1|study_member_id) +
                      (1|shift_day),
                    data = df
)

sjPlot::tab_model(TW_t.lmer)
#### Team Transition GAMLSS
TWt.fit <- gamlss::fitDist(df$tps_transition, type = 'realAll')
TWt.fit$fits
TWt.hist <- gamlss::histDist(tps_transition, family = SEP1, nbins = 30, data = df)

TWL_t.m.gamlss <- gamlss::gamlss(
  formula = tps_transition ~ time_point +
    stressors_total + 
    EDA_Empath + 
    # EDA_Driver +
    HR_Empath +
    # HR_Driver +
    # EDA_Se +
    # HR_Se +
    re(random = ~1|study_member_id) +
    re(random = ~1|shift_day),
  family = SEP1(),
  data = df,trace = FALSE,
  control = gamlss.control(n.cyc = 2000)
)
## Evaluate
summary(WL_t.m.gamlss)
Rsq(WL_t.m.gamlss)
plot(WL_t.m.gamlss)
wp(WL_t.m.gamlss)
TW_a.lmer <- lmer(tps_action ~ 
                    time_point +
                    stressors_total +
                    EDA_Empath +
                    # EDA_Driver +
                    HR_Empath +
                    # HR_Driver +
                    EDA_Se +
                    HR_Se +
                    (1|study_member_id) +
                    (1|shift_day),
                  data = df
)

sjPlot::tab_model(TW_a.lmer)













M.1 <- lm(mayo_total ~ 
              # time_point +
              # stressors_total +
              HR_Empath +
              HR_Driver +
              EDA_Empath +
              EDA_Driver +
              EDA_Se +
              HR_Se,# +
              # (1|study_member_id) + (1|shift_day),
            data = df
)
