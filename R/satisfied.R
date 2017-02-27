# analysis of nces household survey data 
# looking at family satisfaction w/ schools
# 2012 pfi survey data downloaded from nces on 2017-02-13
# https://nces.ed.gov/nhes/dataproducts.asp

# import data/packages ####

# load packages 
library(rio)
library(tidyverse)
library(scales)

# load data
survey_data <- import("data/pfi_pu_pert.sas7bdat")

# clean data ####
colnames(survey_data) <- tolower(colnames(survey_data))

moving_data <- survey_data %>%
  select(path, scpubpri, schoicex, schrtschl, sneighbrx,
         spubchoix, sconsidr, s1stchoi, seenjoy, segrades,
         seadplcx, sebehavx, fcschool, fcteachr, fcstds,
         fcorder, fcsupprt, chispan, casian, cblack,
         cwhite, chisprm, p1sex, p1mrsta, p1educ, p1agepar, 
         p2sex, p2mrsta, p2educ, p2agepar, hwelftan, hwelfst, 
         hwic, hfoodst, hmedicaid, hchip, hsecn8, ttlhhinc,
         yrsaddr, ownrnthb, pargradex, raceeth2, ziplocl, fpwt) %>%
  mutate(move_status = factor(sneighbrx, labels = c("Skip"
                                                    , "Moved", "Did not move")),
         consider_options = factor(sconsidr, labels = c("Skip","Yes", "No")),
         satisfied_sch = factor(fcschool, labels = c("Skip", "Very\nsatisfied", 
                                                     "Somewhat\nsatisfied",
                                                     "Somewhat\ndissatisfied", 
                                                     "Very\ndissatisfied")),
         satisfied_teach = factor(fcteachr, labels = c("Skip", "Very\nsatisfied", 
                                                     "Somewhat\nsatisfied",
                                                     "Somewhat\ndissatisfied", 
                                                     "Very\ndissatisfied")),
         satisfied_stds = factor(fcstds, labels = c("Skip", "Very\nsatisfied", 
                                                     "Somewhat\nsatisfied",
                                                     "Somewhat\ndissatisfied", 
                                                     "Very\ndissatisfied")),
         satisfied_order = factor(fcorder, labels = c("Skip", "Very\nsatisfied", 
                                                     "Somewhat\nsatisfied",
                                                     "Somewhat\ndissatisfied", 
                                                     "Very\ndissatisfied")),
         satisfied_interact = factor(fcsupprt, 
                                     labels = c("Skip", "Very\nsatisfied", 
                                                     "Somewhat\nsatisfied",
                                                     "Somewhat\ndissatisfied", 
                                                     "Very\ndissatisfied")),
         first_choice = factor(s1stchoi, labels = c("Skip", "Yes", "No")),
         public_choices = factor(spubchoix, labels = c("Skip", "Yes", "No",
                                                       "Don't know")),
         charter = factor(schrtschl, labels = c("Skip", "Yes", "No")),
         income = factor(ttlhhinc, 
                         labels = c("<10k",
                                    "10k-20k",
                                    "20k-30k",
                                    "30k-40k", 
                                    "40k-50k", 
                                    "50k-60k", 
                                    "60k-75k",
                                    "75k-100k", 
                                    "100k-150k",
                                    "150k+")),
         race = factor(raceeth2, labels = c("White", "Black", "Hispanic",
                                            "Asian", "Other")),
         zip_grp = factor(ziplocl, labels = c("City, large", "City, medium",
                                              "City, small", "Suburb, large", 
                                              "Suburb, medium", "Suburb, small",
                                              "Town, fringe", "Town, distant", 
                                              "Town, remote", "Rural, fringe",
                                              "Rural, distant", "Rural, remote")))

summarise_groups <- function(df, ...){
  df %>%
    group_by(...) %>%
    summarise(raw_total = n(),
              total = sum(fpwt)) %>%
    mutate(grp_pct = total / sum(total),
           raw_total = raw_total * (53437922 / 17563))
  
}

# How satisfied are parents with their child's school? ####

sch_sat <- moving_data %>% 
  summarise_groups(move_status, satisfied_sch)


ggplot(sch_sat %>% filter(move_status != "Skip"),
       aes(x = satisfied_sch, y = grp_pct, fill = move_status))+
  geom_col(position = "dodge")


move_inc_sch_sat <- moving_data %>%
  summarise_groups(move_status, income, satisfied_sch)


ggplot(move_inc_sch_sat %>% filter(move_status != "Skip"),
       aes(x = satisfied_sch, y = grp_pct, fill = income))+
  geom_col(position = "dodge") +
  facet_wrap(~move_status)

# How satisfied are parents with their child's teacher? ####

teach_sat <- moving_data %>% 
  summarise_groups(move_status, income, satisfied_teach)


ggplot(teach_sat %>% filter(move_status != "Skip"),
       aes(x = satisfied_teach, y = grp_pct, fill = income))+
  geom_col(position = "dodge")+
  facet_wrap(~move_status)

# How satisfied are parents with the academic standards? ####

stds_sat <- moving_data %>% 
  summarise_groups(move_status, satisfied_stds)

# How satisfied are parents with school discipline? ####

order_sat <- moving_data %>% 
  summarise_groups(move_status, satisfied_order)

# How satisfied are parents with school staff parent interaction/outreach? #### 

interact_sat <- moving_data %>% 
  summarise_groups(move_status, satisfied_interact)
