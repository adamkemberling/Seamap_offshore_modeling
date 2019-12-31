# Getting to Sex Ratios from GSMFC data

#Load Packages
library(here)
library(tidyverse)
library(readxl)
library(odbc)
library(dbplyr)
library(ggridges)

name_match <- function(df1, df2) {names(df1)[which(names(df1) %in% names(df2))]}

#Load and format data
mycon <- dbConnect(odbc::odbc(),
                   DBQ = here("data/ms_access_db", "public_seamap.mdb"),
                   Driver = "{Microsoft Access Driver (*.mdb, *.accdb)};")

# # To disconnect database :
# dbDisconnect(mycon)


# Catch Data
bgsrec <- tbl(mycon, "BGSREC") %>% collect() %>%  rename_all(tolower)
bgsrec <- bgsrec %>% rename(biocode = bio_bgs)

# Cruise Data
cruises <- tbl(mycon, "CRUISES") %>% collect() %>% rename_all(tolower)
cruises <- cruises %>%
  select(vessel, cruiseid, cruise_no,  yr, title, source) %>% 
  filter(str_detect(tolower(title), "groundfish"), 
         str_detect(tolower(title), "fall") | str_detect(tolower(title), "summer"))

# Indv. Measurements
glfrec <- tbl(mycon, "GLFREC") %>% collect() %>% rename_all(tolower)
glfrec <- glfrec %>% 
  select(vessel,
         cruise_no,
         stationid,
         p_sta_no,
         biocode = bio_glf,
         indvl_wt,
         meas_cd = meascd_glf,
         indvl_len = len_glf,
         indvl_sex = sex_glf)


new_biocodes <- tbl(mycon, "NEWBIOCODESBIG") %>% collect() %>% rename_all(tolower)
new_biocodes <- new_biocodes %>% select(
  biocode = code,
  taxonomic,
  common_name
)


name_match(bgsrec, new_biocodes)
bgsrec <- left_join(bgsrec, new_biocodes, by = "biocode")




####  Make merges  ####
name_match(glfrec, bgsrec)
bgs_glf <- bgsrec %>% full_join(glfrec, by = c("vessel", "cruise_no",  "stationid", "p_sta_no", "biocode")) %>% 
  select(vessel, cruiseid, cruise_no, stationid, p_sta_no,
         biocode, genus_bgs, spec_bgs, common_name, bgscode, cnt, cntexp, sample_bgs, select_bgs, nodc_bgs, is_sample,
         invrecid, indvl_wt, indvl_len, meas_cd, indvl_sex)

# Contains all species
name_match(bgs_glf, cruises)
bgs_glf_cruises <- bgs_glf %>% right_join(cruises, by = c("vessel", "cruiseid", "cruise_no")) %>% 
  mutate(yr = forcats::fct_rev(yr))

#Plotting a single species lengths/widths with ridgeplots
bgs_glf_cruises %>% 
  filter(spec_bgs == "SAPIDU") %>% 
  ggplot(aes(y = yr, x = indvl_len, fill = factor(..quantile..))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = c(0.025, 0.975)) +
  scale_fill_manual(
    name = "Probability", values = c("#FF0000A0", "#A0A0A0A0", "#0000FFA0"),
    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")
  )  +
  ggtitle("C. sapidus - Carapace Width frequencies") + ylab("") + xlab("")



#Sex Ratios for select years
bgs_glf_cruises %>% 
  filter(spec_bgs == "SAPIDU") %>% 
  mutate(yr = as.numeric(as.character(yr))) %>% 
  filter(yr >= 2009) %>% 
  mutate(indvl_sex = ifelse(indvl_sex == "F", "Female", indvl_sex),
         indvl_sex = ifelse(indvl_sex == "M", "Male", indvl_sex),
         indvl_sex = ifelse(indvl_sex %in% c("Female", "Male"), indvl_sex, NA)) %>% 
  count(indvl_sex)

bgs_glf_cruises %>% 
  filter(spec_bgs == "SAPIDU") %>% 
  mutate(yr = as.numeric(as.character(yr))) %>% 
  filter(yr >= 2009) %>% 
  mutate(indvl_sex = ifelse(indvl_sex == "F", "Female", indvl_sex),
         indvl_sex = ifelse(indvl_sex == "M", "Male", indvl_sex),
         indvl_sex = ifelse(indvl_sex %in% c("Female", "Male"), indvl_sex, NA)) %>% 
  summarise(n = n(),
            Females = sum(indvl_sex == "Female", na.rm = T),
            Males = sum(indvl_sex == "Male", na.rm = T),
            `Perc F` = (Females/n) * 100,
            `Perc M` = (Males/n) * 100)



