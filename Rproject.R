#install.packages(c("survey", "srvyr","lubridate", "broom"))

library (tidyverse)
library(janitor)
library(survey)
library(srvyr)
library(lubridate)
library(broom)

year <- 2021


load(paste0("MMEPS_", year, "/office.RData"))
load(paste0("MEPS_", year, "/prescription.RData"))
load(paste0("MEPS_", year, "/demographic.RData"))
load(paste0("MEPS_", year, "/condition.RData"))

#checking if loaded properly
#class(demographic)
#class(condition)
#class(office)
#class(prescription)
#ls()


# Make sure condition data is clean
cond <- condition %>% clean_names()

# Check codes
table(cond$condidx) %>% head()  # "108" usually == migraine

# Get unique persons with migraine
migraine_cond_ids <- cond %>%
  filter(cccodex == "G43") %>%  
  distinct(dupersid) %>%
  mutate(has_migraine_dx = TRUE)

# After creating migraine_cond_ids
head(migraine_cond_ids)
table(migraine_cond_ids$has_migraine_dx)

  
#file_demographic <- paste0("MEPS_", year, "/h233.dat")
#file_condition <- paste0("MEPS_", year, "/h231.dat")
#file_rx <- paste0("MEPS_", year, "/h223a.s")
#file_office <- paste0("MEPS_", year, "/h229g.ssp")
names_demographic <- names(demographic)
head(names(demographic), 20)

#merge by unique person ID assigned in sampling (dupersid)
#left_join keeps all rows in demo dataset plus matching rows from the right
#merged <- demo %>%
#  left_join(mcq, by = "seqn") %>%
#  left_join(rx, by = "seqn")

#create tibble of migraine patients
#migraine ICD-10 code is "G43" in HC-231 documentation
#migraine_patients <- merged %>%
#  filter(str_detect(icd10cdx, "^G43")) #keeps rows if the icd10cdx column starts with "G43"

