# analysis of nces household survey data 
# looking at familes that did not move to their neighborhood
# to send their kids to a better school
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

# Who are the movers? ####

movers <- moving_data %>%
  summarise_groups(move_status)

ggplot(movers, aes(x = move_status, y = total))+
  geom_col(position = "dodge") +
  scale_y_continuous(label = comma)

ggplot(movers, aes(x = move_status, y = grp_pct))+
  geom_col(position = "dodge") +
  scale_y_continuous(label = percent)

# moving by race

race_move <- moving_data %>%
  summarise_groups(race, move_status) 

ggplot(race_move %>% filter(move_status != "Skip"),
       aes(x = race, y = total, fill = move_status))+
  geom_col(position = "dodge") +
  scale_y_continuous(label = comma)+
  labs(x = "Race", y = "Students", fill = "",
       title = "Racial makeup of movers")

ggsave("figures/race_move.png", width = 6, height = 4, units = "in")

ggplot(race_move %>% filter(move_status == "Moved"),
       aes(x = race, y = grp_pct, fill = race))+
  geom_col(position = "dodge") +
  scale_y_continuous(label = percent)+
  labs(x = "Race", y = "Percent of students\nby race", fill = "Race",
       title = "Do moving rates vary by race?")

ggsave("figures/race_move_pct.png", width = 6, height = 4, units = "in")

# moving by income

inc_move <- moving_data %>%
  summarise_groups(income, move_status)

ggplot(inc_move %>% filter(move_status != "Skip"),
       aes(x = income, y = total, fill = move_status))+
  geom_col(position = "dodge") +
  scale_y_continuous(label = comma)+
  labs(x = "Household income in $", y = "Students", fill = "",
       title = "Economic makeup of movers")+
  theme(axis.text.x = element_text(angle = 90, vjust = .5))

ggsave("figures/inc_move.png", width = 6, height = 4, units = "in")


ggplot(inc_move %>% filter(move_status == "Moved"),
       aes(x = income, y = grp_pct))+
  geom_col() +
  scale_y_continuous(label = percent)+
  labs(x = "Household income in $", y = "Percent of students\nby income",
       title = "Moving rate by household income")+
  theme(axis.text.x = element_text(angle = 90, vjust = .5))

ggsave("figures/inc_move_pct.png", width = 6, height = 4, units = "in")


# movers by race & income

race_inc_move <- moving_data %>%
  summarise_groups(move_status, race, income) 

ggplot(race_inc_move%>% filter(move_status == "Moved"),
       aes(x = income, y = total, fill = race))+  
  geom_col(position = "stack")+
  scale_y_continuous(label = comma)+
  labs(x = "Household income in $", y = "Students", fill = "Race",
       title = "Economic and racial makeup of movers")+
  theme(axis.text.x = element_text(angle = 90, vjust = .5))

ggsave("figures/race_inc_move.png", width = 6, height = 4, units = "in")

ggplot(race_inc_move %>% filter(move_status == "Moved"),
       aes(x = income, y = grp_pct, fill = race))+ 
  geom_col(position = "stack")+
  scale_y_continuous(label = percent)+
  facet_wrap(~race, ncol = 2)+
  labs(x = "Household income in $", y = "Percent of students", fill = "Race",
       title = "Moving rate by race and household income")+
  theme(axis.text.x = element_text(angle = 90, vjust = .5))

ggsave("figures/race_inc_move_pct.png", width = 6, height = 4, units = "in")

# Where are the movers moving to? ####

zip_move <- moving_data %>%
  summarise_groups(zip_grp, move_status)

ggplot(zip_move %>% filter(move_status == "Moved"), 
       aes(x = zip_grp, y = total))+
  geom_col()+
  scale_y_continuous(label = comma)+
  theme(axis.text.x = element_text(angle =90, vjust = .5))+
  labs(x = "Zip code category", y = "Students",
       title = "Mover destinations")

ggsave("figures/zip_move.png", width = 6, height = 4, units = "in")

# by race

zip_race_move <- moving_data %>%
  summarise_groups(zip_grp, race, move_status)

ggplot(zip_race_move %>% filter(move_status == "Moved"), 
       aes(x = zip_grp, y = total, fill = race))+
  geom_col()+
  scale_y_continuous(label = comma)+
  theme(axis.text.x = element_text(angle =90, vjust = .5))+
  labs(x = "Zip code category", y = "Students", fill = "Race",
       title = "Mover destinations by race")

ggsave("figures/zip_race_move.png", width = 6, height = 4, units = "in")

ggplot(zip_race_move %>% filter(move_status == "Moved"), 
       aes(x = zip_grp, y = total, fill = race))+
  geom_col()+
  scale_y_continuous(label = comma)+
  theme(axis.text.x = element_text(angle =90, vjust = .5),
        legend.position = "none") +
  facet_wrap(~race, ncol =2)+
  labs(x = "Zip code category", y = "Students", fill = "Race",
       title = "Mover destinations by race")

ggsave("figures/zip_race_move_wrap.png", width = 6, height = 6, units = "in")


race_move_zip <- moving_data %>%
  summarise_groups(race, move_status, zip_grp)

ggplot(race_move_zip %>% filter(move_status == "Moved"), 
       aes(x = zip_grp, y = grp_pct, fill = race))+
  geom_col()+
  scale_y_continuous(label = percent)+
  theme(axis.text.x = element_text(angle =90, vjust = .5),
        legend.position = "none") +
  facet_wrap(~race, ncol =2)+
  labs(x = "Zip code category", y = "Percent of students", fill = "Race",
       title = "Moving destination rate by race")

ggsave("figures/zip_race_move_pct.png", width = 6, height = 6, units = "in")

# by income

inc_move_zip<- moving_data %>%
  summarise_groups(income, move_status, zip_grp)

ggplot(inc_move_zip %>% filter(move_status == "Moved"),
       aes(x = income, y = total, fill = zip_grp))+
  geom_col()+
  facet_wrap(~zip_grp, ncol = 3)+
  scale_y_continuous(label = comma)+
  theme(axis.text.x = element_text(angle =90, vjust = .5),
        legend.position = "none")+
  labs(x = "Household income in $", y = "Students", 
       title = "Mover income by destination type")

ggsave("figures/inc_move_zip.png", width = 6, height = 6, units = "in")

ggplot(inc_move_zip %>% filter(move_status == "Moved"),
       aes(x = zip_grp, y = grp_pct, fill = zip_grp))+
  geom_col(position = "dodge")+
  facet_wrap(~income, ncol = 3)+
  scale_y_continuous(label = percent)+
  theme(axis.text.x = element_text(angle =90, vjust = .5),
        legend.position = "none") +
  labs(x = "Zip code category", y = "Percent of students", fill = "",
       title = "Moving destination rate by income")

ggsave("figures/inc_move_zip_pct.png", width = 6, height = 6, units = "in")