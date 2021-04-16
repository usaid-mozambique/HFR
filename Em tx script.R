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
# IMPORT ECHO SUBMISSION
# NOTE THAT THE MONTH COLUMN NEEDS TO BE IN 5 DIGIT EXCEL FORMAT IN ORDER FOR CODE TO RUN WITHOUT RETURNING ERRORS.  

df0 <- read_excel("data_source/AJUDA_NewStructure_Mar16_Revised.xlsx", 
                                       sheet = "Feb_Jul2020")

df1 <- read_excel("data_source/AJUDA_NewStructure_Mar16_Revised.xlsx", 
                  sheet = "Aug_Dec2020")

df2 <- read_excel("data_source/AJUDA_NewStructure_Mar16_Revised.xlsx", 
                  sheet = "Jan_Jun2021")

#-----------------------------------------------------------------------------------
# UNION DATA FROM IMPORTS

df <- dplyr::bind_rows(df0, df1, df2)

#-----------------------------------------------------------------------------------
# COERCE 5 DIGIT NUMBER TO DATE AND REMOVE UNNEEDED VARIABLES

df <- df %>% mutate(Date = excel_numeric_to_date(Months, date_system = "modern")) %>%
  dplyr::select(-c(Months, `Source.Name`, HF_Export, province_HF)) %>%
  dplyr::rename(Orgunituid = DATIM_code,
                Site = `Health Facility`,
                AJUDA_phase = Type)

#-----------------------------------------------------------------------------------
# PRINT DATAFRAME TO DISK

write_excel_csv(
  df,
  "~/R/r_projects/Hfr/output/em_erdsd.csv",
  na = "NA",
  append = FALSE,
  delim = ",",
  quote_escape = "double",
  eol = "\n"
)
