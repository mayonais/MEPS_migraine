#install.packages(c("survey", "srvyr","lubridate", "broom"))

library (tidyverse)
library(janitor)
library(survey)
library(srvyr)
library(lubridate)
library(broom)

year <- 2021

# load files from directory (saved from ascii_2021.R)
load(paste0("MEPS_", year, "/office.RData"))
load(paste0("MEPS_", year, "/prescription.RData"))
load(paste0("MEPS_", year, "/demographic.RData"))
load(paste0("MEPS_", year, "/condition.RData"))

# check if loaded properly
class(condition)
ls()

cond <- condition %>% clean_names()

# check medical condition variable icd10cdx
table(cond$icd10cdx) %>% head()

# get unique persons (dupersid) with migraine (icd10cdx = "G43")
migraine_cond_ids <- cond %>%
  filter(str_starts(icd10cdx, "G43")) %>%  
  distinct(dupersid) %>%
  mutate(has_migraine_dx = TRUE)

# check whether table only shows persons with migraine diagnosis
head(migraine_cond_ids)
table(migraine_cond_ids$has_migraine_dx)

pmed <- prescription %>% clean_names()

# saves unique drug names into a csv in working directory
unique_drugs <- unique(pmed$rxdrgnam)
length(unique_drugs)
write.csv(unique_drugs, "unique_drugs_2021.csv", row.names = FALSE)

preventative_names <- c(
  "TOPIRAMATE", #anti-seizure
  "MEMANTINE", 
  "PROPRANOLOL",#beta-blocker
  "NORTRIPTYLINE", #tricyclic antidepressant
  "AMITRIPTYLINE", #tricyclic antidepressant
  "DIVALPROEX SODIUM"
)

# indicates which persons are on preventatives (1=true, 0=false)
names(pmed)
preventative_users <- pmed %>%
  filter(rxdrgnam %in% preventative_names) %>%
  distinct(dupersid) %>%
  mutate(any_preventative = 1)

# check how many persons are on each drug
pmed %>%
  filter(rxdrgnam == "DIVALPROEX SODIUM") %>% #can change the drug name
  summarize(n_users = n_distinct(dupersid))

# covert to character type for merging 
migraine_cond_ids <- migraine_cond_ids %>%
  mutate(dupersid = as.character(dupersid))

# merged table 
migraine_analysis <- migraine_cond_ids %>%
  left_join(preventative_users, by = "dupersid") %>%
  mutate(any_preventative = replace_na(any_preventative, 0))

# create drug_type variable
migraine_analysis <- migraine_analysis %>%
  mutate(
    drug_type = case_when(
      dupersid %in% pmed$dupersid[pmed$rxdrgnam == "TOPIRAMATE"] ~ "Topiramate",
      dupersid %in% pmed$dupersid[pmed$rxdrgnam == "MEMANTINE"] ~ "Memantine",
      dupersid %in% pmed$dupersid[pmed$rxdrgnam == "PROPRANOLOL"] ~ "Propranolol",
      dupersid %in% pmed$dupersid[pmed$rxdrgnam == "NORTRIPTYLINE"] ~ "Nortriptyline",
      dupersid %in% pmed$dupersid[pmed$rxdrgnam == "AMITRIPTYLINE"] ~ "Amitriptyline",
      dupersid %in% pmed$dupersid[pmed$rxdrgnam == "DIVALPROEX SODIUM"] ~ "Divalproex",
      TRUE ~ "No_preventative"
    )
  )

# merge demographics
migraine_analysis <- migraine_analysis %>%
  left_join(demographic %>% clean_names(), by = "dupersid")

# remove non-key individuals don't receive person-level weights 
migraine_analysis <- migraine_analysis %>%
  filter(keyness == 1, !is.na(perwt21f))

# clean variables
# note to self: panel (2021=23)
migraine_analysis <- migraine_analysis %>%
  select(
    dupersid,
    any_preventative,
    drug_type,
    age21x, # age
    sex,
    racethx, # race
    povcat21, # poverty category
    inscov21, # insurance
    varpsu, # primary sampling unit: which people were sampled together
    varstr, # strata: which group this person was sampled from
    perwt21f # person-level weight variable
  ) %>%
  mutate(
    age21x = as.numeric(age21x),
    # make sure number is treated as categorical
    sex = factor(sex, levels = c(1,2),
                 labels = c("Male", "Female")),
    inscov21 = factor(inscov21, levels = 1:3,
                       labels = c("Private", "Public", "Uninsured")),
    racethx = factor(racethx, levels = 1:5,
                  labels = c("Hispanic", "White", "Black", "Asian", "Other/Multiple")),
    povcat21 = factor(povcat21, levels = 1:5, 
                           labels = c("Poor", "Near Poor", "Low Income", "Middle Income", "High Income"))
  )

options(survey.lonely.psu = "adjust")  # handles one PSU error

# applies the weights, strata, and PSU so MEPS = national population
migraine_design <- svydesign(
  ids = ~varpsu,
  strata = ~varstr,
  weights = ~perwt21f,
  data = migraine_analysis,
  nest = TRUE # PSUs are treated as nested within strata 
)

# any_preventative = dependent; age, sex, insurance, poverty_level = independent
svyglm(any_preventative ~ age21x + sex + inscov21 + povcat21,
       design = migraine_design,
       family = quasibinomial()) #using over binomial due to extra variability

# exponentiate converts log-odds to odds ratios; conf.int = 95% confidence intervals
tidy(model_any, exponentiate = TRUE, conf.int = TRUE)
#write_csv("migraine_svyglm_results.csv")

