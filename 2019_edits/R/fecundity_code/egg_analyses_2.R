#New analyses to add
#This script has been transferred to eggmodels_markdown, newest updates are there

"Analyses I’d like to add:
Egg size ~ CW*season (if we have fall egg sizes)
Egg size ~ CW*region (depending on if we got ovig crabs from all regions
Fecundity ~ CW*region
	Regions:  	S. Texas (Matagorda Bay south)
			TX/LA: (between Matagorda Bay and MS River delta)
			MS Bight: LA east of MS River, MS, AL
			Florida
Percent developing normally ~ molt stage
"

library(tidyverse, quietly = TRUE)
library(broom)
library(here)
library(rnaturalearth)
library(sf)

#Here is the crab data, one row per crab
#crabdat <- read_csv("~/Dropbox (The Craboratory)/The Craboratory/Kemberling/SEAMAP_2017/2017_allcrabs_onerow_withstationdata.csv", guess_max = 10000, col_types = cols())
crabdat <- read_csv(here("data/repo/2017_allcrabs_onerow_withstationdata.csv"), guess_max = 10000, col_types = cols())

#Or the data pre processed with a row for each egg measurement
#eggdat <- read_csv("~/Dropbox (The Craboratory)/The Craboratory/Kemberling/SEAMAP_2017/Reproductive_analysis/seamap_summerandfall2017_crabs.csv", guess_max = 10000, col_types = cols())
eggdat <- read_csv(here("data/repo/seamap_summerandfall2017_crabs.csv"), guess_max = 10000, col_types = cols())
eggdat <- eggdat %>% 
  mutate(Egg_stage = factor(eggdat$Egg_stage),
         molt_stage = factor(molt_stage)
         )

#######################  Model 1 -  Estimated Fecundity non-bayesian ###################


#Some quick formatting
eggdat <- eggdat %>% 
  mutate(Estimated_fecundity = ifelse(Estimated_fecundity == "#DIV/0!", NA, Estimated_fecundity),
         Estimated_fecundity = as.numeric(Estimated_fecundity))

#Zack wants to see if regions matter, so lets add them in now, gonna need the station info data though
#stat_dat <- read_csv("~/Dropbox (The Craboratory)/The Craboratory/Kemberling/SEAMAP_2017/2017_allcrabs_onerow_withstationdata.csv", guess_max = 10000, col_types = cols())
stat_dat <- read_csv(here("data/repo/2017_allcrabs_onerow_withstationdata.csv"), guess_max = 10000, col_types = cols())

#verify that the unique ID's are the same, they were made in a separate script
stat_dat$Unique_ID %in% eggdat$Unique_ID
eggdat$Unique_ID %in% stat_dat$Unique_ID

#Pull the columns we want out, merge them to eggdat by the station info
eggdat <- stat_dat %>%  
  select(Unique_ID, Agency, STA_TIME, END_TIME,STA_LON, STA_LAT) %>% 
  right_join(eggdat, by = "Unique_ID")

#basemap for reference
#usmap <- map_data("state")
usmap <- ne_states(country = "united states of america") %>% 
  st_as_sf() %>% 
  filter(region == "South")

#show where they are
 
ggplot() + 
  geom_point(data = eggdat, aes(STA_LON, STA_LAT)) + 
  #geom_polygon(data = usmap, aes(long, lat, group = group)) + 
  geom_sf(data = usmap, fill = "antiquewhite1") + 
  coord_sf(xlim = c(-82, -97.5), ylim = c(25.9, 31)) +
  labs(x= "", y = "") +
  theme_minimal()

#Make regions
eggdat <- eggdat %>% 
  mutate(region = ifelse(STA_LAT < 28.387595 & STA_LON < -96, "South Texas", "TX/LA"),
         region = ifelse(STA_LON > -89.2, "MS Bight", region),
         region = ifelse(STA_LON > -87.5, "FL", region),
         region = factor(region))

#check by plotting
ggplot() + 
  geom_point(data = eggdat, aes(STA_LON, STA_LAT, color = region)) + 
  geom_sf(data = usmap, fill = "antiquewhite1") + 
  coord_sf(xlim = c(-82, -97.5), ylim = c(25.9, 31)) +
  labs(x= "", y = "") +
  theme_minimal()

#Check to see if hand calculations (Estimated_fecundity) = programatic estimates (mean_fecundity)
eggdat %>% filter(ovigerous == "yes") %>% group_by(Unique_ID) %>% 
  summarise(mean_fecundity = mean(Estimated_fecundity, na.rm = TRUE)) %>% 
  right_join(eggdat, by = "Unique_ID") %>% 
  select(Unique_ID, Estimated_fecundity, mean_fecundity)

#We cool, just use Estimated_fecundity, but pull one entry for each crab
fecunddat <- eggdat %>% 
  filter(ovigerous == "yes") %>% 
  distinct(Unique_ID, .keep_all = TRUE)

#Plot relationships with different link functions
#Gaussian
glm(Estimated_fecundity ~ Carapace_width, data = fecunddat)

fecunddat %>% 
  ggplot(aes(Carapace_width, Estimated_fecundity)) + 
  geom_point() + geom_smooth(method = "glm", 
                             formula = y~x,
                             method.args = list(family = gaussian(link = 'identity')))
#Log link
glm(Estimated_fecundity ~ Carapace_width, data = fecunddat, family = gaussian(link = "log"))

fecunddat %>% 
  ggplot(aes(Carapace_width, Estimated_fecundity)) + 
  geom_point(color = "gray20") + 
  geom_smooth(method = "glm", 
              formula = y~x,
              method.args = list(family = gaussian(link = 'log')),
              color = "black",
              linetype = 2) +
  theme_classic() +
  xlab("Carapace Width (mm)") +
  ylab("Estimated Fecundity (Number of Eggs)")

#Zack wants to see if regions matter, so lets add them in now
m1 <- glm(Estimated_fecundity ~ Carapace_width + region, data = fecunddat, family = gaussian(link = "log"))
summary(m1)
tidy(m1)

#and plot
fecunddat %>% 
  ggplot(aes(Carapace_width, Estimated_fecundity)) + 
  geom_point(aes(color = region)) + 
  geom_smooth(method = "glm", 
              formula = y~x,
              method.args = list(family = gaussian(link = 'log')),
              color = "black",
              linetype = 2) +
  theme_classic() +
  xlab("Carapace Width (mm)") +
  ylab("Estimated Fecundity (Number of Eggs)")

#facet wrap region tosee if it looks relevant
fecunddat %>% 
  ggplot(aes(Carapace_width, Estimated_fecundity)) + 
  geom_point() + 
  geom_smooth(method = "glm", 
              formula = y~x,
              method.args = list(family = gaussian(link = 'log')),
              color = "black",
              linetype = 2) +
  theme_classic() +
  xlab("Carapace Width (mm)") +
  ylab("Estimated Fecundity (Number of Eggs)") +
  facet_wrap(~region)

##################  New analysis 1 - Egg size ~ CW*season ###########################
#Egg size ~ CW*season (if we have fall egg sizes), we don't have enough
#So we have repeated measures for measurements on each egg mass, 
#so gonna have the eggmass/crabID as a random effect as they are likely correlated

# # Use the fecundity data that isn't onerow per crab
# eggdat <- read_csv("~/Dropbox (The Craboratory)/The Craboratory/Kemberling/SEAMAP_2017/Reproductive_analysis/seamap_summerandfall2017_crabs.csv")


p1<- ggplot(eggdat) +
  geom_point(aes(Carapace_width, egg_volume, color = Unique_ID), alpha = 0.5, show.legend = FALSE) +
  facet_wrap(~Egg_stage)
plotly::ggplotly(p1)

p1 <- ggplot(eggdat) + geom_col(aes(x = egg_volume, y =  Egg_stage, fill = Egg_stage))
plotly::ggplotly(p1)

# #So a few crabs have egg volumes much higher than others, gonna look more closely at them
# eggdat %>% filter(Unique_ID %in% c("X14574", "X14553", "X14576", "X14874", "X14552")) %>% 
#   select(Unique_ID, eggdiam_1_ou, eggdiam_2_ou, diam1_mm, diam2_mm, egg_volume) %>% 
#   View()

#Looks like the calculated volumes might be deviating from 4/3*pi*r^3, so I'm gonna check that here
eggdat <- eggdat %>% 
  mutate(new_vol = (4/3) * pi * ((diam1_mm + diam2_mm) / 4) ^ 3) %>%
  filter(is.na(Egg_stage) == FALSE)

p1 <- eggdat %>% 
  ggplot() + 
  geom_point(aes(egg_volume, new_vol, color = Unique_ID), show.legend = FALSE) 
plotly::ggplotly(p1)


#So the values that appeared much higher have the correct volume calculations, the others have something different
#back to the original exploration plots but with the new_vol
p1 <- ggplot(eggdat) +
  geom_point(aes(Carapace_width, new_vol, color = Unique_ID), alpha = 0.5, show.legend = FALSE) +
  facet_wrap(~Egg_stage)
plotly::ggplotly(p1)

#Boxplots
eggdat %>% 
  mutate(Egg_stage = factor(Egg_stage)) %>% 
ggplot(aes(Egg_stage, new_vol)) + geom_boxplot() +ylab("Volume mm^3")

eggdat %>% 
  ggplot(aes(Carapace_width, new_vol)) + geom_point(aes(color = factor(Egg_stage))) +
  geom_smooth(method = "lm")

#so without repeated measures for each crab
m1 <- lm(new_vol ~ Carapace_width, data = eggdat)
summary(m1)
tidy(m1)


#We have 100 eggs for fall which is only 5 individuals
eggdat %>% count(Season)

#
m2 <- lme4::lmer(new_vol ~ Carapace_width + (1 | Unique_ID), data = eggdat)
summary(m2)
tidy(m2) %>% View("eggsize ~ CW + CrabID")


##################  New analysis 2  ###########################
#Egg size ~ CW*region (depending on if we got ovig crabs from all regions

# #Use the fecundity data that isn't onerow per crab
# eggdat <- read_csv("~/Dropbox (The Craboratory)/The Craboratory/Kemberling/SEAMAP_2017/Reproductive_analysis/seamap_summerandfall2017_crabs.csv")


#Plot relationship first
eggdat %>% 
  ggplot(aes(Carapace_width, new_vol)) +
  geom_point(aes(color = region)) +
  geom_smooth(method = "glm",
              formula = y~x,
              method.args = list(family = gaussian(link = 'identity')))
  
#region seems viable from here, but likely an interaction with egg stage
eggdat %>% #facet egg stages
  ggplot(aes(Carapace_width, new_vol)) +
  geom_point(aes(color = region)) +
  facet_wrap(~Egg_stage)

eggdat %>% #facet regions
  ggplot(aes(Carapace_width, new_vol)) +
  geom_point(aes(color = Egg_stage)) +
  facet_wrap(~region)


m1 <- glm(new_vol ~ Carapace_width * Egg_stage * region, 
          data = eggdat,
          family = gaussian(link = "identity"))
m1_summ <- tidy(m1)
View(m1_summ)



##################  New analysis 3  ###########################
#Fecundity ~ CW*region
#Regions:  	S. Texas (Matagorda Bay south)
#TX/LA: (between Matagorda Bay and MS River delta)
#MS Bight: LA east of MS River, MS, AL
#Florida

#Look at the region facet again
fecunddat %>% 
  ggplot(aes(Carapace_width, Estimated_fecundity)) + 
  geom_point() + 
  geom_smooth(method = "glm", 
              formula = y~x,
              method.args = list(family = gaussian(link = 'log')),
              color = "black",
              linetype = 2) +
  theme_classic() +
  xlab("Carapace Width (mm)") +
  ylab("Estimated Fecundity (Number of Eggs)") +
  facet_wrap(~region)

#Very weird results here, non significant impact of any
m1 <- glm(Estimated_fecundity ~ Carapace_width * region, 
          data = fecunddat,
          family = gaussian(link = "log"))
summary(m1)
tidy(m1) %>% View("Fecund ~ CW * region")



##################  New analysis 4  ###########################
#Percent developing normally ~ molt stage
#We measured at an eggmass level so use fecunddat


fecunddat %>% 
  ggplot(aes(molt_stage, Percent_fert)) +
  geom_boxplot()

aov(Percent_fert ~ molt_stage, data = fecunddat)
#Percent fertilizaiton did not vary significantly with molt stage





