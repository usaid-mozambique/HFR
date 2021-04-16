#-----------------------------------------------------------------------------------
##  LOAD CORE TIDYVERSE & OTHER PACKAGES
library(readr)
library(tidyr)
library(dplyr)
library(tibble)
library(stringr)
library(lubridate)
library(janitor)
library(readxl)

rm(list = ls())

#-----------------------------------------------------------------------------------
# PATHS

input <- "C:/Users/cnhantumbo/Documents/USAID HFR"

#-----------------------------------------------------------------------------------
# IMPORT DATASET
# DATASET SOURCE: MONTHLY ECHO PREP SUBMISSION THAT ACCOMPANIES ER/DSD SUBMISSION

#prep_compile <- read_excel("data_source/prep_compile.xlsx", .name_repair = "universal") %>%
prep_compile <- read_excel("C:/Users/cnhantumbo/Documents/USAID HFR/prep_compile.xlsx", .name_repair = "universal") %>%  
glimpse()

#-----------------------------------------------------------------------------------
# PROCESS DATASET & CREATE ENHANCED MONITORING DATAFRAME

em_prep <- prep_compile %>%
  tidyr::pivot_longer(cols= !c(Site, DATIM_Code, Month), names_to = "indicator", values_to = "value") %>% 
  dplyr::mutate(
    sex = dplyr::case_when(grepl("_female", indicator) ~ "Female",
                               grepl("_male", indicator) ~ "Male"),
    age = dplyr::case_when(grepl("_15_19", indicator) ~ "15-19",
                               grepl("_20_24", indicator) ~ "20-24",
                               grepl("_25_49", indicator) ~ "25-49",
                               grepl("_50.", indicator) ~ "50+"),
    poptype = dplyr::case_when(grepl("PWID", indicator) ~ "PWID",
                               grepl("MSM", indicator) ~ "MSM",
                               grepl("TG", indicator) ~ "TG",
                               grepl("FSW", indicator) ~ "FSW",
                               grepl("Pregnant.breastfeeding.women", indicator) ~ "P/LW",
                               grepl("SDC", indicator) ~ "Serodiscordant Couples",
                               grepl("People.in.prison.and.other.closed.settings", indicator) ~ "Prisoners etc.",
                               grepl("AGYW", indicator) ~ "AGYW",
                               grepl("Custom", indicator) ~ "Other"),
    indicator_2 = dplyr::case_when(grepl("Number.of.clients.HIV.tested.for.PrEP.initiation", indicator) ~ "PrEP_SCREEN",
                                   grepl("Number.of.clients.screened.for.initiation.testing.negative", indicator) ~ "PrEP_ELIGIBLE",
                                   grepl("Number.of.clients.initiating.PrEP.for.the.first.time", indicator) ~ "PrEP_NEW_VERIFY",
                                   grepl("Number.of.clients.returning.for.1.month..initial", indicator) ~ "PrEP_1MONTH",
                                   grepl("Number.of.clients.returning.for.any.subsequent.follow.up.visits", indicator) ~ "PrEP_RETURN_ALL",
                                   grepl("Number.of.clients.restarting.PrEP", indicator) ~ "PrEP_RESTART",
                                   grepl("Number.of.seroconversions", indicator) ~ "PrEP_SEROCON"),
    agecoarse = dplyr::case_when(age == "<1" ~ "<15",
                                 age == "1-9" ~ "<15",
                                 age == "10-14" ~ "<15"),
    agecoarse = replace_na(agecoarse, "15+")
  ) %>%
  tidyr::drop_na(indicator_2) %>%
  dplyr::filter(value != 0) %>%
  dplyr::select(-c(indicator)) %>%
  dplyr::rename(indicator = indicator_2,
                site = Site,
                date = Month) %>%
  dplyr::select(c(date, site, indicator, sex, age, agecoarse, poptype, value))

#-----------------------------------------------------------------------------------
# SUBSET DATAFRAME TO CREATE HFR SUBMISSION

hfr_prep <- em_prep %>%
  dplyr::rename(val = value,
               orgunit = site) %>%
  dplyr::mutate(operatingunit = "Mozambique",
                mech_code = "70212",
                partner = "ECHO",
                indicator= dplyr::recode(indicator, "PrEP_NEW_VERIFY" = "PrEP_NEW")) %>%
  dplyr::filter(indicator == "PrEP_NEW")


#-----------------------------------------------------------------------------------
# PRINT EXCEL DOCUMENTS TO COMPUTER

openxlsx::write.xlsx(em_prep, file = "C:/Users/cnhantumbo/Documents/USAID HFR/em_prep.xlsx", sheetName = "em_prep")
openxlsx::write.xlsx(hfr_prep, file = "C:/Users/cnhantumbo/Documents/USAID HFR/hfr_prep.xlsx", sheetName = "hfr_prep")

unique(em_prep[c("age")])
unique(em_prep[c("sex")])
unique(em_prep[c("poptype")])
unique(em_prep[c("indicator")])
