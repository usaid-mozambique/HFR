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

FY20Q3 = "6/20/2020"
FY20Q4 = "9/20/2020"
FY21Q1 = "12/20/2020"

#-----------------------------------------------------------------------------------
# IP SUBMISSION TAB NAMES

# Revised Data Entry Form Q1FY21
# Revised Data Entry Form Q3FY20
# Revised Data Entry Form Q4FY20

#-----------------------------------------------------------------------------------
# IMPORT FY21Q1 DATASET - NOTE THAT THE FOLLOWING NEED UPDATES WITH EACH NEW SUBMISSION - 1) OBJECT NAME; 2) REFERENCE PERIOD; 3) FILE NAME (WHEN APPLICABLE)

#-----------------------------------------------------------------------------------
# IMPORT ARIEL FILE




ARIEL_Q1FY21 <- read_excel("~/USAID/00. SI/00. HIV/01. M&E/00. Data/96. Ad Hoc (e.g. KP MISAU)/00. KP/Final Data/Final Data/ARIEL_KeyPop Data Request.xlsx", 
                           sheet = "Revised Data Entry Form Q1FY21", 
                           col_types = c("text", "text", "text", 
                                         "text", "text", "text", "text", "text", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric"), 
                           skip = 7) %>%
  dplyr::filter(`Implementing Partner` == "ARIEL") %>%
  dplyr::mutate(period = FY21Q1) %>%
  dplyr::filter(!is.na(TX_NEW_Total))


test_N <- ARIEL_Q1FY21 %>%
  filter(Province == "Maputo Provincia")
sum(test_N$TX_PVLS_N_Total)

test_D <- ARIEL_Q1FY21 %>%
  filter(Province == "Maputo Provincia")
sum(test_D$TX_PVLS_D_TOTAL)

##### TEST
test_N <- ARIEL_Q1FY21 %>%
  filter(Province == "Cabo Delgado")
sum(test_N$TX_PVLS_N_Total)

##### TEST
test_D <- ARIEL_Q1FY21 %>%
  filter(Province == "Cabo Delgado")
sum(test_D$TX_PVLS_D_TOTAL)


#-----------------------------------------------------------------------------------
# IMPORT CCS FILE

CCS_Q1FY21 <- read_excel("~/USAID/00. SI/00. HIV/01. M&E/00. Data/96. Ad Hoc (e.g. KP MISAU)/00. KP/Final Data/Final Data/CCS_KeyPop Data Request.xlsx", 
                         sheet = "Revised Data Entry Form Q1FY21", 
                         col_types = c("text", "text", "text", 
                                       "text", "text", "text", "text", "text", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric"), 
                         skip = 7) %>%
  dplyr::filter(`Implementing Partner` == "CCS") %>%
  dplyr::mutate(period = FY21Q1) %>%
  dplyr::filter(!is.na(TX_NEW_Total))


test_N <- CCS_Q1FY21 %>%
  filter(Province == "Maputo Cidade")
sum(test_N$TX_PVLS_N_Total)

##### TEST
test_D <- CCS_Q1FY21 %>%
  filter(Province == "Maputo Cidade")
sum(test_D$TX_PVLS_D_TOTAL)


#-----------------------------------------------------------------------------------
# IMPORT EGPAF FILE

EGPAF_Q1FY21 <- read_excel("~/USAID/00. SI/00. HIV/01. M&E/00. Data/96. Ad Hoc (e.g. KP MISAU)/00. KP/Final Data/Final Data/EGPAF_KeyPop Data Request_Q3_20 Q4_20 e Q1_21_16 02 2021_.xlsx", 
                           sheet = "Revised Data Entry Form Q1FY21", 
                           col_types = c("text", "text", "text", 
                                         "text", "text", "text", "text", "text", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric"), 
                           skip = 7) %>%
  dplyr::filter(`Implementing Partner` == "EGPAF") %>%
  dplyr::mutate(period = FY21Q1) %>%
  dplyr::filter(!is.na(TX_NEW_Total))

test_N <- EGPAF_Q1FY21 %>%
  filter(Province == "Gaza")
sum(test_N$TX_PVLS_N_Total)

##### TEST
test_D <- EGPAF_Q1FY21 %>%
  filter(Province == "Gaza")
sum(test_D$TX_PVLS_D_TOTAL)


test_N <- EGPAF_Q1FY21 %>%
  filter(Province == "Inhambane")
sum(test_N$TX_PVLS_N_Total)

##### TEST
test_D <- EGPAF_Q1FY21 %>%
  filter(Province == "Inhambane")
sum(test_D$TX_PVLS_D_TOTAL)


#-----------------------------------------------------------------------------------
# IMPORT FGH FILE

FGH_Q1FY21 <- read_excel("~/USAID/00. SI/00. HIV/01. M&E/00. Data/96. Ad Hoc (e.g. KP MISAU)/00. KP/Final Data/Final Data/FGH_KeyPop Data Request_120221.xlsx", 
                         sheet = "Revised Data Entry Form Q1FY21", 
                         col_types = c("text", "text", "text", 
                                       "text", "text", "text", "text", "text", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric"), 
                         skip = 7) %>%
  dplyr::filter(`Implementing Partner` == "FGH") %>%
  dplyr::mutate(period = FY21Q1) %>%
  dplyr::filter(!is.na(TX_NEW_Total))

test_N <- FGH_Q1FY21 %>%
  filter(Province == "Zambezia")
sum(test_N$TX_PVLS_N_Total)

##### TEST
test_D <- FGH_Q1FY21 %>%
  filter(Province == "Zambezia")
sum(test_D$TX_PVLS_D_TOTAL)

#-----------------------------------------------------------------------------------
# IMPORT ICAP FILE

ICAP_Q1FY21 <- read_excel("~/USAID/00. SI/00. HIV/01. M&E/00. Data/96. Ad Hoc (e.g. KP MISAU)/00. KP/Final Data/Final Data/ICAP_KeyPop Data Request.xlsx", 
                          sheet = "Revised Data Entry Form Q1FY21", 
                          col_types = c("text", "text", "text", 
                                        "text", "text", "text", "text", "text", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric"), 
                          skip = 7) %>%
  dplyr::filter(`Implementing Partner` == "ICAP") %>%
  dplyr::mutate(period = FY21Q1) %>%
  dplyr::filter(!is.na(TX_NEW_Total))

test_N <- ICAP_Q1FY21 %>%
  filter(Province == "Nampula")
sum(test_N$TX_PVLS_N_Total)

##### TEST
test_D <- ICAP_Q1FY21 %>%
  filter(Province == "Nampula")
sum(test_D$TX_PVLS_D_TOTAL)


#-----------------------------------------------------------------------------------
# IMPORT ECHO FILES

ECHO_N_Q1FY21 <- read_excel("~/USAID/00. SI/00. HIV/01. M&E/00. Data/96. Ad Hoc (e.g. KP MISAU)/00. KP/Final Data/Final Data/KeyPop Data Request_Niassa_ECHO.xlsx", 
                            sheet = "Revised Data Entry Form Q1FY21", 
                            col_types = c("text", "text", "text", 
                                          "text", "text", "text", "text", "text", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric"), 
                            skip = 7) %>%
  dplyr::filter(`Implementing Partner` == "ECHO",
                Province == "Niassa") %>%
  dplyr::mutate(period = FY21Q1) %>%
  dplyr::filter(!is.na(TX_NEW_Total))


ECHO_M_Q1FY21 <- read_excel("~/USAID/00. SI/00. HIV/01. M&E/00. Data/96. Ad Hoc (e.g. KP MISAU)/00. KP/Final Data/Final Data/KeyPop Data Request_Manica_ECHO_Manica.xlsx", 
                            sheet = "Revised Data Entry Form Q1FY21", 
                            col_types = c("text", "text", "text", 
                                          "text", "text", "text", "text", "text", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric"), 
                            skip = 7) %>%
  dplyr::filter(`Implementing Partner` == "ECHO",
                Province == "Manica") %>%
  dplyr::mutate(period = FY21Q1) %>%
  dplyr::filter(!is.na(TX_NEW_Total))


ECHO_S_Q1FY21 <- read_excel("~/USAID/00. SI/00. HIV/01. M&E/00. Data/96. Ad Hoc (e.g. KP MISAU)/00. KP/Final Data/Final Data/KeyPop Data Request_Sofala_ECHO.xlsx", 
                            sheet = "Revised Data Entry Form Q1FY21", 
                            col_types = c("text", "text", "text", 
                                          "text", "text", "text", "text", "text", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric"), 
                            skip = 7) %>%
  dplyr::filter(`Implementing Partner` == "ECHO",
                Province == "Sofala") %>%
  dplyr::mutate(period = FY21Q1) %>%
  dplyr::filter(!is.na(TX_NEW_Total))


ECHO_T_Q1FY21 <- read_excel("~/USAID/00. SI/00. HIV/01. M&E/00. Data/96. Ad Hoc (e.g. KP MISAU)/00. KP/Final Data/Final Data/KeyPop Data Request_18_02_2021_TETE.xlsx", 
                            sheet = "Revised Data Entry Form Q1F21", 
                            col_types = c("text", "text", "text", 
                                          "text", "text", "text", "text", "text", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric"), 
                            skip = 7) %>%
  dplyr::filter(`Implementing Partner` == "ECHO",
                Province == "Tete") %>%
  dplyr::mutate(period = FY21Q1) %>%
  dplyr::filter(!is.na(TX_NEW_Total))


test_N <- ECHO_T_Q1FY21 %>%
  filter(Province == "Tete")
sum(test_N$TX_PVLS_N_Total)

##### TEST
test_D <- ECHO_T_Q1FY21 %>%
  filter(Province == "Tete")
sum(test_D$TX_PVLS_D_Total)

test_N <- ECHO_S_Q1FY21 %>%
  filter(Province == "Sofala")
sum(test_N$TX_PVLS_N_Total)

##### TEST
test_D <- ECHO_S_Q1FY21 %>%
  filter(Province == "Sofala")
sum(test_D$TX_PVLS_D_Total)

test_N <- ECHO_N_Q1FY21 %>%
  filter(Province == "Niassa")
sum(test_N$TX_PVLS_N_Total)

##### TEST
test_D <- ECHO_N_Q1FY21 %>%
  filter(Province == "Niassa")
sum(test_D$TX_PVLS_D_Total)

test_N <- ECHO_M_Q1FY21 %>%
  filter(Province == "Manica")
sum(test_N$TX_PVLS_N_Total)

##### TEST
test_D <- ECHO_M_Q1FY21 %>%
  filter(Province == "Manica")
sum(test_D$TX_PVLS_D_Total)


Q1FY21 <- dplyr::bind_rows(ARIEL_Q1FY21, CCS_Q1FY21, ICAP_Q1FY21, FGH_Q1FY21, EGPAF_Q1FY21, ECHO_N_Q1FY21, ECHO_M_Q1FY21, ECHO_S_Q1FY21, ECHO_T_Q1FY21)



#-----------------------------------------------------------------------------------
# IMPORT FY20Q4 DATASET - NOTE THAT THE FOLLOWING NEED UPDATES WITH EACH NEW SUBMISSION - 1) OBJECT NAME; 2) REFERENCE PERIOD; 3) FILE NAME (WHEN APPLICABLE)

#-----------------------------------------------------------------------------------
# IMPORT ARIEL FILE

ARIEL_Q4FY20 <- read_excel("~/USAID/00. SI/00. HIV/01. M&E/00. Data/96. Ad Hoc (e.g. KP MISAU)/00. KP/Final Data/Final Data/ARIEL_KeyPop Data Request.xlsx", 
                           sheet = "Revised Data Entry Form Q4FY20", 
                           col_types = c("text", "text", "text", 
                                         "text", "text", "text", "text", "text", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric"), 
                           skip = 7) %>%
  dplyr::filter(`Implementing Partner` == "ARIEL") %>%
  dplyr::mutate(period = FY20Q4) %>%
  dplyr::filter(!is.na(TX_NEW_Total))

#-----------------------------------------------------------------------------------
# IMPORT CCS FILE

CCS_Q4FY20 <- read_excel("~/USAID/00. SI/00. HIV/01. M&E/00. Data/96. Ad Hoc (e.g. KP MISAU)/00. KP/Final Data/Final Data/CCS_KeyPop Data Request.xlsx", 
                         sheet = "Revised Data Entry Form Q4FY20", 
                         col_types = c("text", "text", "text", 
                                       "text", "text", "text", "text", "text", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric"), 
                         skip = 7) %>%
  dplyr::filter(`Implementing Partner` == "CCS") %>%
  dplyr::mutate(period = FY20Q4) %>%
  dplyr::filter(!is.na(TX_NEW_Total))

#-----------------------------------------------------------------------------------
# IMPORT EGPAF FILE

EGPAF_Q4FY20 <- read_excel("~/USAID/00. SI/00. HIV/01. M&E/00. Data/96. Ad Hoc (e.g. KP MISAU)/00. KP/Final Data/Final Data/EGPAF_KeyPop Data Request_Q3_20 Q4_20 e Q1_21_16 02 2021_.xlsx", 
                           sheet = "Revised Data Entry Form Q4FY20", 
                           col_types = c("text", "text", "text", 
                                         "text", "text", "text", "text", "text", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric"), 
                           skip = 7) %>%
  dplyr::filter(`Implementing Partner` == "EGPAF") %>%
  dplyr::mutate(period = FY20Q4) %>%
  dplyr::filter(!is.na(TX_NEW_Total))

#-----------------------------------------------------------------------------------
# IMPORT FGH FILE

FGH_Q4FY20 <- read_excel("~/USAID/00. SI/00. HIV/01. M&E/00. Data/96. Ad Hoc (e.g. KP MISAU)/00. KP/Final Data/Final Data/FGH_KeyPop Data Request_120221.xlsx", 
                         sheet = "Revised Data Entry Form Q4FY20", 
                         col_types = c("text", "text", "text", 
                                       "text", "text", "text", "text", "text", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric"), 
                         skip = 7) %>%
  dplyr::filter(`Implementing Partner` == "FGH") %>%
  dplyr::mutate(period = FY20Q4) %>%
  dplyr::filter(!is.na(TX_NEW_Total))

#-----------------------------------------------------------------------------------
# IMPORT ICAP FILE

ICAP_Q4FY20 <- read_excel("~/USAID/00. SI/00. HIV/01. M&E/00. Data/96. Ad Hoc (e.g. KP MISAU)/00. KP/Final Data/Final Data/ICAP_KeyPop Data Request.xlsx", 
                          sheet = "Revised Data Entry Form Q4FY20", 
                          col_types = c("text", "text", "text", 
                                        "text", "text", "text", "text", "text", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric"), 
                          skip = 7) %>%
  dplyr::filter(`Implementing Partner` == "ICAP") %>%
  dplyr::mutate(period = FY20Q4) %>%
  dplyr::filter(!is.na(TX_NEW_Total))

#-----------------------------------------------------------------------------------
# IMPORT ECHO FILES

ECHO_N_Q4FY20 <- read_excel("~/USAID/00. SI/00. HIV/01. M&E/00. Data/96. Ad Hoc (e.g. KP MISAU)/00. KP/Final Data/Final Data/KeyPop Data Request_Niassa_ECHO.xlsx", 
                            sheet = "Revised Data Entry Form Q4FY20", 
                            col_types = c("text", "text", "text", 
                                          "text", "text", "text", "text", "text", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric"), 
                            skip = 7) %>%
  dplyr::filter(`Implementing Partner` == "ECHO",
                Province == "Niassa") %>%
  dplyr::mutate(period = FY20Q4) %>%
  dplyr::filter(!is.na(TX_NEW_Total))


ECHO_M_Q4FY20 <- read_excel("~/USAID/00. SI/00. HIV/01. M&E/00. Data/96. Ad Hoc (e.g. KP MISAU)/00. KP/Final Data/Final Data/KeyPop Data Request_Manica_ECHO_Manica.xlsx", 
                            sheet = "Revised Data Entry Form Q4FY20", 
                            col_types = c("text", "text", "text", 
                                          "text", "text", "text", "text", "text", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric"), 
                            skip = 7) %>%
  dplyr::filter(`Implementing Partner` == "ECHO",
                Province == "Manica") %>%
  dplyr::mutate(period = FY20Q4) %>%
  dplyr::filter(!is.na(TX_NEW_Total))


ECHO_S_Q4FY20 <- read_excel("~/USAID/00. SI/00. HIV/01. M&E/00. Data/96. Ad Hoc (e.g. KP MISAU)/00. KP/Final Data/Final Data/KeyPop Data Request_Sofala_ECHO.xlsx", 
                            sheet = "Revised Data Entry Form Q4FY20", 
                            col_types = c("text", "text", "text", 
                                          "text", "text", "text", "text", "text", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric"), 
                            skip = 7) %>%
  dplyr::filter(`Implementing Partner` == "ECHO",
                Province == "Sofala") %>%
  dplyr::mutate(period = FY20Q4) %>%
  dplyr::filter(!is.na(TX_NEW_Total))


ECHO_T_Q4FY20 <- read_excel("~/USAID/00. SI/00. HIV/01. M&E/00. Data/96. Ad Hoc (e.g. KP MISAU)/00. KP/Final Data/Final Data/KeyPop Data Request_18_02_2021_TETE.xlsx", 
                            sheet = "Revised Data Entry Form Q4F20", 
                            col_types = c("text", "text", "text", 
                                          "text", "text", "text", "text", "text", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric"), 
                            skip = 7) %>%
  dplyr::filter(`Implementing Partner` == "ECHO",
                Province == "Tete") %>%
  dplyr::mutate(period = FY20Q4) %>%
  dplyr::filter(!is.na(TX_NEW_Total))

Q4FY20 <- dplyr::bind_rows(ARIEL_Q4FY20, CCS_Q4FY20, ICAP_Q4FY20, FGH_Q4FY20, EGPAF_Q4FY20, ECHO_N_Q4FY20, ECHO_M_Q4FY20, ECHO_S_Q4FY20, ECHO_T_Q4FY20)


#-----------------------------------------------------------------------------------
# IMPORT FY20Q3 DATASET - NOTE THAT THE FOLLOWING NEED UPDATES WITH EACH NEW SUBMISSION - 1) OBJECT NAME; 2) REFERENCE PERIOD; 3) FILE NAME (WHEN APPLICABLE)

#-----------------------------------------------------------------------------------
# IMPORT ARIEL FILE

ARIEL_Q3FY20 <- read_excel("~/USAID/00. SI/00. HIV/01. M&E/00. Data/96. Ad Hoc (e.g. KP MISAU)/00. KP/Final Data/Final Data/ARIEL_KeyPop Data Request.xlsx", 
                           sheet = "Revised Data Entry Form Q3FY20", 
                           col_types = c("text", "text", "text", 
                                         "text", "text", "text", "text", "text", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric"), 
                           skip = 7) %>%
  dplyr::filter(`Implementing Partner` == "ARIEL") %>%
  dplyr::mutate(period = FY20Q3) %>%
  dplyr::filter(!is.na(TX_NEW_Total))

#-----------------------------------------------------------------------------------
# IMPORT CCS FILE

CCS_Q3FY20 <- read_excel("~/USAID/00. SI/00. HIV/01. M&E/00. Data/96. Ad Hoc (e.g. KP MISAU)/00. KP/Final Data/Final Data/CCS_KeyPop Data Request.xlsx", 
                         sheet = "Revised Data Entry Form Q3FY20", 
                         col_types = c("text", "text", "text", 
                                       "text", "text", "text", "text", "text", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric"), 
                         skip = 7) %>%
  dplyr::filter(`Implementing Partner` == "CCS") %>%
  dplyr::mutate(period = FY20Q3) %>%
  dplyr::filter(!is.na(TX_NEW_Total))

#-----------------------------------------------------------------------------------
# IMPORT EGPAF FILE

EGPAF_Q3FY20 <- read_excel("~/USAID/00. SI/00. HIV/01. M&E/00. Data/96. Ad Hoc (e.g. KP MISAU)/00. KP/Final Data/Final Data/EGPAF_KeyPop Data Request_Q3_20 Q4_20 e Q1_21_16 02 2021_.xlsx", 
                           sheet = "Revised Data Entry Form Q3FY20", 
                           col_types = c("text", "text", "text", 
                                         "text", "text", "text", "text", "text", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric"), 
                           skip = 7) %>%
  dplyr::filter(`Implementing Partner` == "EGPAF") %>%
  dplyr::mutate(period = FY20Q3) %>%
  dplyr::filter(!is.na(TX_NEW_Total))

#-----------------------------------------------------------------------------------
# IMPORT FGH FILE

FGH_Q3FY20 <- read_excel("~/USAID/00. SI/00. HIV/01. M&E/00. Data/96. Ad Hoc (e.g. KP MISAU)/00. KP/Final Data/Final Data/FGH_KeyPop Data Request_120221.xlsx", 
                         sheet = "Revised Data Entry Form Q3FY20", 
                         col_types = c("text", "text", "text", 
                                       "text", "text", "text", "text", "text", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric"), 
                         skip = 7) %>%
  dplyr::filter(`Implementing Partner` == "FGH") %>%
  dplyr::mutate(period = FY20Q3) %>%
  dplyr::filter(!is.na(TX_NEW_Total))

#-----------------------------------------------------------------------------------
# IMPORT ICAP FILE

ICAP_Q3FY20 <- read_excel("~/USAID/00. SI/00. HIV/01. M&E/00. Data/96. Ad Hoc (e.g. KP MISAU)/00. KP/Final Data/Final Data/ICAP_KeyPop Data Request.xlsx", 
                          sheet = "Revised Data Entry Form Q3FY20", 
                          col_types = c("text", "text", "text", 
                                        "text", "text", "text", "text", "text", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric"), 
                          skip = 7) %>%
  dplyr::filter(`Implementing Partner` == "ICAP") %>%
  dplyr::mutate(period = FY20Q3) %>%
  dplyr::filter(!is.na(TX_NEW_Total))

#-----------------------------------------------------------------------------------
# IMPORT ECHO FILES

ECHO_N_Q3FY20 <- read_excel("~/USAID/00. SI/00. HIV/01. M&E/00. Data/96. Ad Hoc (e.g. KP MISAU)/00. KP/Final Data/Final Data/KeyPop Data Request_Niassa_ECHO.xlsx", 
                            sheet = "Revised Data Entry Form Q3FY20", 
                            col_types = c("text", "text", "text", 
                                          "text", "text", "text", "text", "text", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric"), 
                            skip = 7) %>%
  dplyr::filter(`Implementing Partner` == "ECHO",
                Province == "Niassa") %>%
  dplyr::mutate(period = FY20Q3) %>%
  dplyr::filter(!is.na(TX_NEW_Total))


ECHO_M_Q3FY20 <- read_excel("~/USAID/00. SI/00. HIV/01. M&E/00. Data/96. Ad Hoc (e.g. KP MISAU)/00. KP/Final Data/Final Data/KeyPop Data Request_Manica_ECHO_Manica.xlsx", 
                            sheet = "Revised Data Entry Form Q3FY20", 
                            col_types = c("text", "text", "text", 
                                          "text", "text", "text", "text", "text", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric"), 
                            skip = 7) %>%
  dplyr::filter(`Implementing Partner` == "ECHO",
                Province == "Manica") %>%
  dplyr::mutate(period = FY20Q3) %>%
  dplyr::filter(!is.na(TX_NEW_Total))


ECHO_S_Q3FY20 <- read_excel("~/USAID/00. SI/00. HIV/01. M&E/00. Data/96. Ad Hoc (e.g. KP MISAU)/00. KP/Final Data/Final Data/KeyPop Data Request_Sofala_ECHO.xlsx", 
                            sheet = "Revised Data Entry Form Q3FY20", 
                            col_types = c("text", "text", "text", 
                                          "text", "text", "text", "text", "text", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric"), 
                            skip = 7) %>%
  dplyr::filter(`Implementing Partner` == "ECHO",
                Province == "Sofala") %>%
  dplyr::mutate(period = FY20Q3) %>%
  dplyr::filter(!is.na(TX_NEW_Total))


ECHO_T_Q3FY20 <- read_excel("~/USAID/00. SI/00. HIV/01. M&E/00. Data/96. Ad Hoc (e.g. KP MISAU)/00. KP/Final Data/Final Data/KeyPop Data Request_18_02_2021_TETE.xlsx", 
                            sheet = "Revised Data Entry Form Q3FY20", 
                            col_types = c("text", "text", "text", 
                                          "text", "text", "text", "text", "text", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric"), 
                            skip = 7) %>%
  dplyr::filter(`Implementing Partner` == "ECHO",
                Province == "Tete") %>%
  dplyr::mutate(period = FY20Q3) %>%
  dplyr::filter(!is.na(TX_NEW_Total))

Q3FY20 <- dplyr::bind_rows(ARIEL_Q3FY20, CCS_Q3FY20, ICAP_Q3FY20, FGH_Q3FY20, EGPAF_Q3FY20, ECHO_N_Q3FY20, ECHO_M_Q3FY20, ECHO_S_Q3FY20, ECHO_T_Q3FY20)


#-----------------------------------------------------------------------------------
# UNION ALL COMPILED FILES AND MAKE TIDY

KP_MISAU <- dplyr::bind_rows(Q1FY21, Q4FY20, Q3FY20) %>%
  dplyr::filter(!is.na(TX_NEW_Total)) %>%
  tidyr::gather(temp, Value, TX_NEW_Total:TX_RET_N_REC, na.rm = TRUE) %>%
  dplyr::mutate(KeyPop = dplyr::case_when(grepl("REC", temp) ~ "REC",
                                          grepl("Rec", temp) ~ "REC",
                                       grepl("HSH", temp) ~ "HSH",
                                       grepl("MTS", temp) ~ "MTS",
                                       grepl("PWID", temp) ~ "PWID",
                                       grepl("PID", temp) ~ "PWID",
                                       grepl("Total", temp) ~ "Total",
                                       grepl("TOTAL", temp) ~ "Total"),
                Indicator = dplyr::case_when(grepl("TX_NEW", temp) ~ "TX_NEW",
                                          grepl("TX_CURR", temp) ~ "TX_CURR",
                                          grepl("TX_PVLS_N", temp) ~ "TX_PVLS_N",
                                          grepl("TX_PVLS_D", temp) ~ "TX_PVLS_D",
                                          grepl("TX_RET_N", temp) ~ "TX_RET_N",
                                          grepl("TX_RET_D", temp) ~ "TX_RET_D")) %>%
  dplyr::select(Partner = `Implementing Partner`, Province, District, Site = `DATIM_HF Name`, Orgunituid = DATIM_code, Period_EPTS = `Reporting Period_EPTS`, Period = period, Indicator, KeyPop, Value)

#--------------------------------------------------# PRINT HFR DATAFRAME TO DISK

readr::write_excel_csv2(
  KP_MISAU,
  "~/R/r_projects/Hfr/output/em_kp_misau.csv",
  na = "NA",
  append = FALSE,
  delim = ",",
  quote_escape = "double",
  eol = "\n"
)


