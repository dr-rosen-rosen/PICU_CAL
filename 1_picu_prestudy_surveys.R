library(readr)
library(tidyverse)


picu_study_readingmindeyes <- read_csv("picu_study_readingmindeyes.csv")


picu_study_readingmindeyes <- picu_study_readingmindeyes %>%
  mutate(Pic_1_score = if_else(Pic_1== "playful", 1, 0)) %>%
  mutate(Pic_2_score = if_else(Pic_2== "upset", 1, 0)) %>%
  mutate(Pic_3_score = if_else(Pic_3== "desire", 1, 0)) %>%
  mutate(Pic_4_score = if_else(Pic_4== "insisting", 1, 0)) %>%
  mutate(Pic_5_score = if_else(Pic_5== "worried", 1, 0)) %>%
  mutate(Pic_6_score = if_else(Pic_1== "fantasizing", 1, 0)) %>%
  mutate(Pic_7_score = if_else(Pic_7== "uneasy", 1, 0)) %>%
  mutate(Pic_8_score = if_else(Pic_8== "despondent", 1, 0)) %>%
  mutate(Pic_9_score = if_else(Pic_9== "preoccupied", 1, 0)) %>%
  mutate(Pic_10_score = if_else(Pic_10== "cautious", 1, 0)) %>%
  mutate(Pic_11_score = if_else(Pic_11== "regretful", 1, 0)) %>%
  mutate(Pic_12_score = if_else(Pic_12== "sceptical", 1, 0)) %>%
  mutate(Pic_13_score = if_else(Pic_13== "anticipating", 1, 0)) %>%
  mutate(Pic_14_score = if_else(Pic_14== "accusing", 1, 0)) %>%
  mutate(Pic_15_score = if_else(Pic_15== "contemplative", 1, 0)) %>%
  mutate(Pic_16_score = if_else(Pic_16== "thoughtful", 1, 0)) %>%
  mutate(Pic_17_score = if_else(Pic_17== "doubtful", 1, 0)) %>%
  mutate(Pic_18_score = if_else(Pic_18== "decisive", 1, 0)) %>%
  mutate(Pic_19_score = if_else(Pic_19== "tentative", 1, 0)) %>%
  mutate(Pic_20_score = if_else(Pic_20== "friendly", 1, 0)) %>%
  mutate(Pic_21_score = if_else(Pic_21== "fantasizing", 1, 0)) %>%
  mutate(Pic_22_score = if_else(Pic_22== "preoccupied", 1, 0)) %>%
  mutate(Pic_23_score = if_else(Pic_23== "defiant", 1, 0)) %>%
  mutate(Pic_24_score = if_else(Pic_24== "pensive", 1, 0)) %>%
  mutate(Pic_25_score = if_else(Pic_25== "interested", 1, 0)) %>%
  mutate(Pic_26_score = if_else(Pic_26== "hostile", 1, 0)) %>%
  mutate(Pic_27_score = if_else(Pic_27== "cautious", 1, 0)) %>%
  mutate(Pic_28_score = if_else(Pic_28== "interested", 1, 0)) %>%
  mutate(Pic_29_score = if_else(Pic_29== "reflective", 1, 0)) %>%
  mutate(Pic_30_score = if_else(Pic_30== "flirtatious", 1, 0)) %>%
  mutate(Pic_31_score = if_else(Pic_31== "confident", 1, 0)) %>%
  mutate(Pic_32_score = if_else(Pic_32== "serious", 1, 0)) %>%
  mutate(Pic_33_score = if_else(Pic_33== "concerned", 1, 0)) %>%
  mutate(Pic_34_score = if_else(Pic_34== "distrustful", 1, 0)) %>%
  mutate(Pic_35_score = if_else(Pic_35== "nervous", 1, 0)) %>%
  mutate(Pic_36_score = if_else(Pic_36== "suspicious", 1, 0)) 

picu_study_readingmindeyes <- picu_study_readingmindeyes %>%
  rowwise() %>%
  mutate(total_score = sum(across(Pic_1_score:Pic_36_score)))


write.csv(picu_study_readingmindeyes, "picu_study_readingmindeyes_final.csv")


###############################################################
  

picu_study_trait_raw <- read_csv("picu_study_trait_raw.csv")


#HEXACO scoring

#Doing reverse scoring of items 3, 4, 7, 8, 9, 11, 
#12, 17, 18, 20, 22, 24
picu_study_trait_raw <- picu_study_trait_raw %>%
  mutate(hex_3R =  5- hex_3) %>%
  mutate(hex_4R =  5- hex_4) %>%
  mutate(hex_7R =  5- hex_7) %>%
  mutate(hex_8R =  5- hex_8) %>%
  mutate(hex_9R =  5- hex_9) %>%
  mutate(hex_11R =  5- hex_11) %>%
  mutate(hex_12R =  5- hex_12) %>%
  mutate(hex_17R =  5- hex_17) %>%
  mutate(hex_18R =  5- hex_18) %>%
  mutate(hex_20R =  5- hex_20) %>%
  mutate(hex_22R =  5- hex_22) %>%
  mutate(hex_24R =  5- hex_24)


picu_study_trait_raw <- picu_study_trait_raw %>%
  rowwise() %>%
  mutate(hexaco_honesty = mean(c(hex_6, hex_12R, hex_18R, hex_24R)))%>%
  mutate(hexaco_emotionality = mean(c(hex_5, hex_11R, hex_17R, hex_23)))%>%
  mutate(hexaco_extraversion = mean(c(hex_4R, hex_10, hex_16, hex_22R)))%>%
  mutate(hexaco_agreeableness = mean(c(hex_3R, hex_9R, hex_15, hex_21)))%>%
  mutate(hexaco_conscientiousness = mean(c(hex_2, hex_8R, hex_14, hex_20R)))%>%
  mutate(hexaco_openness = mean(c(hex_1, hex_7R, hex_13, hex_19)))%>%
  mutate(hexaco_total = mean (c(hex_1, hex_2, hex_3R, hex_4R, hex_5, hex_6,
                                hex_7R, hex_8R, hex_9R, hex_10, hex_11R, hex_12R,
                                hex_13, hex_14, hex_15, hex_16, hex_17R, hex_18R,
                                hex_19, hex_20R, hex_21, hex_22R, hex_23, hex_24R)))


#Collective orientation- items 1-10 are for affiliation subscale,
#items 11-15 are for dominance subscale
picu_study_trait_raw <- picu_study_trait_raw %>%
  rowwise() %>%
  mutate(collective_orientation_affiliation = mean(c(co_1, co_2, co_3, co_4, co_5,
                                                     co_6, co_7, co_8, co_9, co_10))) 

picu_study_trait_raw <- picu_study_trait_raw %>%
  mutate(collective_orientation_dominance = mean(c(co_11, co_12, co_13, co_14, co_15))) 

picu_study_trait_raw <- picu_study_trait_raw%>%
  mutate(collective_orientation_total = mean(c(co_1, co_2, co_3, co_4, co_5,
                                             co_6, co_7, co_8, co_9, co_10,
                                             co_11, co_12, co_13, co_14, co_15)))
  
  
write.csv(picu_study_trait_raw, "picu_study_trait_final.csv")









hist(picu_study_trait_raw$hexaco_total)






  

hist(picu_study_readingmindeyes$total_score)
  