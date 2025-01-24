library(tidyverse)
library(ggplot2)
library(patchwork)
library(ggtext)

################## Forrest plots
out_coefs <- readxl::read_excel(here::here('mplus_analyses','picu_coefs_2by4.xlsx'), sheet = 'outcomes_df') |>
  mutate(
    profile = as.factor(profile),
    lower = coef - se*1.96,
    upper = coef + se*1.96,
    outcome = case_match(outcome,
                         'trust_coop_beh' ~ 'Trust: Cooperative behavior',
                         'trust_mon_beh' ~ 'Trust: Mon behavior',
                         'tps_transition' ~ 'Team Process: Transition',
                         'tps_action' ~ 'Team Process: Action',
                         'tps_interpersonal' ~ 'Team Process: Interpersonal',
                         'belief_process' ~ 'Blief process',
                         'belief_potency' ~ 'Blief potency',
                         'mbi' ~ 'Burnout'),
    outcome = factor(outcome,
                     levels = c('Blief process','Team Process: Action','Blief potency',
                                'Trust: Cooperative behavior','Team Process: Transition',
                                'Team Process: Interpersonal','Trust: Mon behavior',
                                'Burnout')))

#define colours for dots and bars
barCOLS = c("#F8766D","#7CAE00","#00BFC4","#C77CFF")
dotCOLS = c("#fcc8c5","#d8ff79","#81fcff","#e9cbff")

outcome.fp <- ggplot(out_coefs, aes(x=outcome, y=coef, ymin=lower, ymax=upper,col=profile,fill=profile)) + 
  #specify position here
  geom_linerange(linewidth=3,position=position_dodge(width = 0.5)) +
  geom_hline(yintercept=0, lty=2) +
  #specify position here too
  geom_point(size=3, shape=21, colour="white", stroke = 0.5,position=position_dodge(width = 0.5)) +
  #scale_fill_manual(values=barCOLS)+
  #scale_color_manual(values=dotCOLS)+
  scale_x_discrete(name="Self-reported team and individual outcomes", limits = rev) +
  scale_y_continuous(name="Coefficients", limits = c(-.6, .6)) +
  #facet_wrap(~var_type, ncol = 1, scales = 'free_y') +
  coord_flip() +
  #ggthemes::theme_tufte()
  theme_minimal() + theme(
    legend.position="bottom") +
  labs(title = 'Effects of L2 Team Member Class on Outcomes')+ 
  guides(fill=guide_legend(title="Team Member Class"), col = guide_legend(title="Team Member Class"))

trait_coefs <- readxl::read_excel(here::here('mplus_analyses','picu_coefs_2by4.xlsx'), sheet = 'traits_df') |>
  mutate(
    profile = as.factor(profile),
    lower = coef - se*1.96,
    upper = coef + se*1.96,
    trait_var = case_match(trait_var,
                           'co_aff' ~ 'Collective Orientation: Affiliation',
                           'co_dom' ~ 'Collective Orientation: Dom',
                           'hx_agr' ~ 'Agreeableness',
                           'hx_con' ~ 'Conscientiousness',
                           'hx_emo' ~ 'Emotionality',
                           'hx_ext' ~ 'Extraversion',
                           'hx_hon' ~ 'Honesty',
                           'hx_opn' ~ 'Openness',
                           'rm_eye_tot' ~ 'Reading the mind in the eyes'),
    trait_var = factor(trait_var, levels = c('Collective Orientation: Affiliation',
                                             'Collective Orientation: Dom', 'Agreeableness',
                                             'Extraversion','Reading the mind in the eyes',
                                             'Openness','Conscientiousness','Emotionality',
                                             'Honesty'))
  )

trait.fp <- ggplot(trait_coefs, aes(x=trait_var, y=coef, ymin=lower, ymax=upper,col=profile,fill=profile)) + 
  #specify position here
  geom_linerange(linewidth=3,position=position_dodge(width = 0.5)) +
  geom_hline(yintercept=0, lty=2) +
  #specify position here too
  geom_point(size=3, shape=21, colour="white", stroke = 0.5,position=position_dodge(width = 0.5)) +
  #scale_fill_manual(values=barCOLS)+
  #scale_color_manual(values=dotCOLS)+
  scale_x_discrete(name="Trait measure", limits = rev) +
  scale_y_continuous(name="Coefficients and 95% CIs", limits = c(-12, 12)) +
  #facet_wrap(~var_type, ncol = 1, scales = 'free_y') +
  coord_flip() +
  #ggthemes::theme_tufte()
  theme_minimal() + theme(
    legend.position="bottom") +
  labs(title = 'Associaiton between trait variables and L2 TPD Classes') + 
  guides(fill=guide_legend(title="Team Member Class"), col = guide_legend(title="Team Member Class"))

min(trait_coefs$lower
    )