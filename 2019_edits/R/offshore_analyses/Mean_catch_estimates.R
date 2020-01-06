

####  Characterizing Offshore habitat use, means by area, survey, and year
# Data 4/30/2019
# Author : Adam Kemberling

#packages
library(MASS)
library(here)
library(tidyverse)
library(knitr)
library(emmeans)
library(gridExtra)
library(broom)
library(statmod)

#Format ggplot for rest of document
theme_set(theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()))

`%not in%` <- purrr::negate(`%in%`)


#Load and clean up data - from IOA_report
# Read in data
seamap <- read_csv("~/Dropbox/SEAMAP_2019/data/offshore/seamap_cpue_2019.csv",
                   guess_max = 1e6, 
                   col_types = cols())

#Set up Season Column, using survey titles
seamap[str_detect(seamap$TITLE, "Fall"),"Season"] <- "Fall"
seamap[str_detect(seamap$TITLE, "Winter"),"Season"] <- "Winter"
seamap[str_detect(seamap$TITLE, "Summer"),"Season"] <- "Summer"
seamap[str_detect(seamap$TITLE, "Spring"),"Season"] <- "Spring"

# Make any setup changes
seamap <- seamap %>% 
  mutate(STAT_ZONE     = factor(STAT_ZONE),
         Crab_presence = ifelse(Sapidus_Catch > 0, 1, 0),
         Survey_Year   = as.numeric(Survey_Year),
         Season        = factor(Season, levels = c("Summer","Fall","Winter")),
         Survey_Month  = lubridate::month(Date),
         Survey_Month  = factor(Survey_Month),
         Survey_design = ifelse(Survey_Year < 2009, "Original", "Updated"),
         Survey_design = ifelse(Survey_Year == 2008 & Season == "Fall", "Updated", Survey_design),
         Survey_design = factor(Survey_design, levels = c("Original", "Updated"))) 

# Do all filtering
seamap <- seamap %>%        #More changes from DE
  mutate(Year_f             =  factor(Survey_Year),
         Month_f            =  factor(Survey_Month),
         Depth              =  (Start_Depth + End_Depth)/2,
         Temp_std           =  (Temp_B- mean(Temp_B))/ mean(Temp_B),
         Salinity_std       =  (Salinity_B- mean(Salinity_B))/ mean(Salinity_B),
         Depth_std          =  (Depth- mean(Depth))/ mean(Depth)) %>% 
  filter(is.na(STAT_ZONE)  ==  FALSE, 
         STAT_ZONE         !=  "NA",
         Season            !=  "Winter",
         Survey_Year     %in%  c(2010:2018),
         #STAT_ZONE   %not in%  c(0, 1, 22, 12),      #Not enough Obs
         #STAT_ZONE   %not in%  c(6,9),
         Spd_kmh           !=  0,                    #these are infinite cpue
         Season            !=  "Winter",
         Depth             <=  110
  )


# Should we remove catch outliers?
seamap %>% 
  mutate(`Outlier Status` = ifelse(Sapidus_Catch > 80, "Large Outlier", "Plausible Catch"),
         `Outlier Status` = ifelse(Sapidus_Catch <= 25, "No Concern", `Outlier Status`),
         `Outlier Status` = factor(`Outlier Status`, levels = c("No Concern", "Plausible Catch", "Large Outlier"))) %>% 
  ggplot() + 
  geom_point(aes(x = Year_f, y = Sapidus_Catch, color = `Outlier Status`)) +
  scale_color_manual(values = c("gray", "orange", "darkred")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  xlab("") + ylab("C. sapidus Catch")


#Look at catch only
crabs <- seamap %>% filter(Sapidus_Catch > 0)

crabs %>% 
  filter(Survey_Year > 2009) %>% 
  mutate(STAT_ZONE = fct_rev(STAT_ZONE)) %>% 
  ggplot(aes(STAT_ZONE, Sapidus_Catch)) + 
  geom_boxplot(aes(group = STAT_ZONE)) + 
  labs(title    = "Blue Crab Catch by Stat Zone", 
       subtitle = "Stat zones ordered West to East - 2010 to 2018", 
       x        = "Statistical Zone",
       y        = "C. sapidus Single Station Catch")





########################  Group_by and plot  ###############################

# Things we want
# mean cpue by year, with sd

# 1. Summary table by year of the mean catch and sd, and the average encounter rate and sd
library(scales)

year_summ <- seamap %>% 
  group_by(Survey_Year) %>% 
  summarise(
    n_obs = n(),
    mean_catch = mean(Sapidus_Catch, na.rm = T),
    catch_se = sd(Sapidus_Catch, na.rm = T)/ sqrt(n_obs),
    perc_occurrence = mean(Crab_presence, na.rm = T),
    occurrence_se = sd(Crab_presence, na.rm = T) / sqrt(n_obs)
  ) %>% 
  ungroup() %>% 
  mutate(Survey_Year = factor(Survey_Year),
         catch_ymin = mean_catch - (2 * catch_se),
         catch_ymax = mean_catch + (2 * catch_se),
         occurrence_ymin = perc_occurrence - (2 * occurrence_se),
         occurrence_ymax = perc_occurrence + (2 * occurrence_se),
         occurrence_ymin = ifelse(occurrence_ymin < 0, 0, occurrence_ymin)
         )

# catch bar chart
p1 <- ggplot(year_summ) +
  geom_col(aes(Survey_Year, mean_catch)) +
  geom_errorbar(aes(x = Survey_Year, 
                    ymin = catch_ymin,
                    ymax = catch_ymax),
                size = .25) +
  labs(x= "Year",
       y = "Mean Catch per tow")

# percent occurence
p2 <- ggplot(year_summ) +
  geom_col(aes(Survey_Year, perc_occurrence)) +
  geom_errorbar(aes(x = Survey_Year, 
                    ymin = occurrence_ymin,
                    ymax = occurrence_ymax),
                size = .25) +
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "Year",
       y = "Occurrence Rate of C. sapidus")

# 2. Again for the Stat Zones, weould be nice to add in empties here
#could just append in summaries I guess :/ or do this before removal
sz_summ <- seamap %>% 
  group_by(STAT_ZONE) %>% 
  summarise(
    n_obs = n(),
    mean_catch = mean(Sapidus_Catch, na.rm = T),
    catch_se = sd(Sapidus_Catch, na.rm = T)/ sqrt(n_obs),
    perc_occurrence = mean(Crab_presence, na.rm = T),
    occurrence_se = sd(Crab_presence, na.rm = T) / sqrt(n_obs)
  ) %>% 
  ungroup() %>% 
  mutate(STAT_ZONE = fct_rev(STAT_ZONE),
         catch_ymin = mean_catch - (2 * catch_se),
         catch_ymax = mean_catch + (2 * catch_se),
         catch_ymin = ifelse(catch_ymin < 0, 0, catch_ymin),
         occurrence_ymin = perc_occurrence - (2 * occurrence_se),
         occurrence_ymax = perc_occurrence + (2 * occurrence_se),
         occurrence_ymin = ifelse(occurrence_ymin < 0, 0, occurrence_ymin)
  )

# catch bar chart
p3 <- ggplot(sz_summ) +
  geom_col(aes(STAT_ZONE, mean_catch)) +
  geom_errorbar(aes(x = STAT_ZONE, 
                    ymin = catch_ymin,
                    ymax = catch_ymax),
                size = .25) +
  labs(x= "Statistical zone (Ordered West to East)",
       y = "Mean Catch per tow")

# percent occurence
p4 <- ggplot(sz_summ) +
  geom_col(aes(STAT_ZONE, perc_occurrence)) +
  geom_errorbar(aes(x = STAT_ZONE, 
                    ymin = occurrence_ymin,
                    ymax = occurrence_ymax),
                size = .25) +
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "Statistical zone (Ordered West to East)",
       y = "Occurrence Rate of C. sapidus")


# 3. and again by season
season_summ <- seamap %>% 
  group_by(Survey_Year, Season) %>% 
  summarise(
    n_obs = n(),
    mean_catch = mean(Sapidus_Catch, na.rm = T),
    catch_se = sd(Sapidus_Catch, na.rm = T)/ sqrt(n_obs),
    perc_occurrence = mean(Crab_presence, na.rm = T),
    occurrence_se = sd(Crab_presence, na.rm = T) / sqrt(n_obs)
  ) %>% 
  ungroup() %>% 
  mutate(Season = factor(Season),
         Survey_Year = factor(Survey_Year),
         catch_ymin = mean_catch - (2 * catch_se),
         catch_ymax = mean_catch + (2 * catch_se),
         catch_ymin = ifelse(catch_ymin < 0, 0, catch_ymin),
         occurrence_ymin = perc_occurrence - (2 * occurrence_se),
         occurrence_ymax = perc_occurrence + (2 * occurrence_se),
         occurrence_ymin = ifelse(occurrence_ymin < 0, 0, occurrence_ymin)
  )


# catch bar chart
p5 <- ggplot(season_summ) +
  geom_col(aes(Survey_Year, mean_catch, fill = Season), position = "dodge") +
  # geom_errorbar(aes(x = Survey_Year, 
  #                   ymin = catch_ymin,
  #                   ymax = catch_ymax,
  #                   group = Season),
  #               size = .25) +
  labs(x= "Sampling Period",
       y = "Mean Catch per tow")

# percent occurence
p6 <- ggplot(season_summ) +
  geom_col(aes(Survey_Year, perc_occurrence, fill = Season), position = "dodge") +
  # geom_errorbar(aes(x = Survey_Year, 
  #                   ymin = occurrence_ymin,
  #                   ymax = occurrence_ymax),
  #               size = .25) +
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "Sampling Period",
       y = "Occurrence Rate of C. sapidus")

# Then probably a summary table of them that doesn't look crummy
grid.arrange(p1, p2, ncol = 2)
grid.arrange(p3, p4, ncol = 2)
grid.arrange(p5, p6, ncol = 2)
# Include the number of observations, mean, and standard deviation


#The janitor package has a function for these frequency tables, I don't remember what it is called though
library(janitor)
tabyl(seamap, STAT_ZONE, Survey_design) %>% adorn_title() %>% kable()

