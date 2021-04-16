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

#Read in PASSOS Tracker
passos <- read_excel("C:/Users/cnhantumbo/Documents/USAID HFR/PASSOS FY2021 Tracker Mensal_Fevereiro 10032021.xlsx", 
                  sheet = "Dados e Metas PC", na = "", col_types = c("text", "text", "text", "text", "text", "skip", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                     "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "skip", "skip", "skip", "skip", "skip"), skip = 2)
glimpse(passos) #review data structure

table(passos$'Data Type')

############################
#SELECT QUARTER
############################
Q <- "Q1"
############################
############################
#SELECT FY
############################
FY <- 2021
############################

passos_formatted <- passos  %>%  
  filter(`Fiscal Year`==FY, `Data Type` == "Result", Quarter == Q) %>%     #filter to FY21 and beyond
                                      #rename variables for more efficient processing in R
  mutate(
    SNU = Province,
    PSNU = District, 
    FY = `Fiscal Year`, 
    TX_PVLS_VERIFY_D = `TX_PVLS_VERIFY (Denominator)`,
    TX_PVLS_VERIFY_N = `TX_PVLS_VERIFY (Numerator)`,
    TX_NEW_VERIFY = KP_TX_NEW_VERIFY, 
    TX_CURR_VERIFY = KP_TX_CURR_VERIFY,
    TX_RTT_VERIFY = KP_TX_RTT_VERIFY,	
    TX_PVLS_ELIGIBLE = KP_TX_VL_ELIGIBLE  
    ) %>%
  
#pivot indicator values from wide to long
  
  pivot_longer(c(TX_NEW_VERIFY, TX_CURR_VERIFY, COMM_SUPP_RET, TX_RTT_VERIFY,
                 TX_PVLS_ELIGIBLE, TX_PVLS_VERIFY_D,	TX_PVLS_VERIFY_N,
                 PrEP_SCREEN,	PrEP_ELIGIBLE,	PrEP_NEW_VERIFY,	PrEP_CURR_VERIFY)) %>%
  
  
  mutate(
#recode pivot output
           indicator = name,
           val = value,
         
#rename Key Populations in accordance with custom indicator format
          Population=recode(`Key Population`,
                           "FSW" = "Female sex workers (FSW)",
                           "MSM" = "Men who have sex with men (MSM)",
                           "PWID" = "People who inject drugs (PWID)",
                           "TG" = "Transgender people (TG)",
                           "Prisoners" = "People in prisons and other closed settings",
                           "Non-KP" = "Non-KP (general population)")) %>%
  
#Keep only Pertinent fields
select (SNU, PSNU, FY, Quarter, Month, Population, indicator, val)  %>%
  
#reassign district to Datim geographies
  mutate(PSNU=recode(PSNU,
                        "Chongoene" = "Chonguene",
                        "Muchungué" = "Chibabava" #these are not in Datim, must be recoded (see 2 steps below)
                         ))  %>%
  
#add mechansism, partner, and OU info     +    N/A fields from custom form
  mutate(orgunit = PSNU,
         mech_code	= 18280,
         partner= "FHI360",
         operatingunit = "Mozambique",
         age = NA,
         sex = NA,
         otherdisaggregate = NA) %>%
  
#code districts to their PSNUuidS from DATIM
  mutate(orgunituid=recode(orgunit,
                           "Alto Molocue" = "SnbIkc5HFdC",
                           "Ancuabe" = "e29LLJAHdD1",
                           "Beira" = "c4qdq9NbKuX",
                           "Bilene" = "kZU2LRwlrWA",
                           "Boane" = "KipLfqm48wI",
                           "Changara" = "vJOqqNfY8zQ",
                           "Chibabava" = "Bp2zX2SlSsT",
                           "Chimoio" = "RTCsjF28gNA",
                           "Chiure" = "xamxMtXf90S",
                           "Chonguene" = "jDWCkBXYllV",
                           "Cuamba" = "IBMUL2QvXz9",
                           "Dondo" = "QJviLU1xwFA",
                           "Gondola" = "X7cjhY4Bip4",
                           "Gurue" = "TNxjWVnFcH3",
                           "Inhambane" = "OgL15C7NJo9",
                           "Inharrime" = "pRR2y748unY",
                           "Inhassoro" = "dOgwIHGxt8L",
                           "Kamavota" = "OSrBvFsAtYz",
                           "Kamaxakeni" = "IYDPRaPGDFu",
                           "Kampfumu" = "oMVRVkQ4d6Q",
                           "Kamubukwana" = "itceRrMEAiB",
                           "Lichinga" = "oWKesP8CeL0",
                           "Limpopo" = "SIgUX4PJgJD",
                           "Mabalane" = "qD019OVbYwC",
                           "Mandimba" = "S97ZWSnm6N8",
                           "Manica" = "j59u9FWJDci",
                           "Massinga" = "zuf9ikWuKWN",
                           "Matola" = "rPJ8B53FKqF",
                           "Maxixe" = "KtlpdTLliJg",
                           "Meconta" = "tXKwBtQqb2o",
                           "Metuge" = "fc2uOqmPmxj",
                           "Milange" = "p1khrkzyj1C",
                           "Moamba" = "b9sP7gLskme",
                           "Mocuba" = "yyR3ZgRyBzQ",
                           "Mogovolas" = "EBH5pfGP97m",
                           "Montepuez" = "gC1CuJI7U4O",
                           "Nacala" = "rzyQVCV9LjG",
                           "Nacala-A-Velha" = "Un8VIqAxgEx",
                           "Namacurra" = "YDiTYO8iXlo",
                           "Nampula" = "jwLNNIw1MjY",
                           "Nicoadala" = "MLWfwGSAExm",
                           "Nlhamankulu" = "Pw3doHDr3sw",
                           "Pemba" = "ekAcXW11CZ1",
                           "Quelimane" = "ZZlLKqhLyve",
                           "Tete" = "tKTswBE0Hyj",
                           "Vanduzi" = "ve5fI4S4kCI",
                           "Vilankulo" = "eN26Kt2lvOM",
                           "Xai-Xai" = "kKXWgaF11TT",
                           .default = "y", .missing = "y"
                           ),
         fundingsource=recode(PSNU,
                     "Chimoio" = "KPIF",
                     "Kamavota" = "KPIF",
                     "Kampfumu" = "KPIF",
                     "Kamubukwana" = "KPIF",
                     "Matola" = "KPIF",
                     "Nacala" = "KPIF",
                     "Nampula" = "KPIF",
                     "Nlhamankulu" = "KPIF",
                     "Quelimane" = "KPIF",
                     .default = "COP"),
         
  #create Numerator Denominator Field
       numdenom=recode(indicator,
                          "TX_PVLS_VERIFY_D" = "Denominator",
                          .default = "Numerator"
                       ),
  
  #Rename indicator fields now that N/D distinguish its elements
       indicator=recode(indicator,
                        "TX_PVLS_VERIFY_D" = "TX_PVLS_VERIFY",
                        "TX_PVLS_VERIFY_N" = "TX_PVLS_VERIFY",
                        ),
  
  #Generate Fiscal Year field for filtering and creating Reporting Period field
       period=recode(FY,
                     "2021" = "FY21",
                     "2022" = "FY22",
                     "2023" = "FY23",
                     "2024" = "FY24",
                     "2025" = "FY25")) %>%
  
  #Create Reporting Period field, Month Name
     mutate(reportingperiod = paste(period, Quarter, sep = " "))  %>% 
  
  
  `names<-`(tolower(names(.))) %>%   #Change field names to lower case

  mutate(month_n = match(month,month.name)) 



#review output
names(passos_formatted)
glimpse(passos_formatted)
table(passos_formatted$numdenom)
table(passos_formatted$indicator)
table(passos_formatted$reportingperiod)
table(passos_formatted$month_n)

#View(psnuuid_tester) #check if any PSNUs aren't matched to Datim
psnuuid_tester <- passos_formatted  %>% 
  filter(orgunituid == "y")


#aggregate values by quarter
moz_kp <- passos_formatted  %>%
  filter(val!= "NA") %>%     #filter to non-nulls
  group_by(fundingsource, reportingperiod,	orgunit,	orgunituid,	mech_code,	partner,	operatingunit,	psnu, indicator,	sex,	age,	population,	otherdisaggregate,	numdenom) %>%
  summarise(val = sum(val))

glimpse(moz_kp)


#-----------------------------------------------------------------------------------
# PRINT HFR SUBMISSION TO DISK

write_excel_csv(
  moz_kp,
  "C:/Users/cnhantumbo/Documents/USAID HFR/ci_kp_passos.csv",
  na = "NA",
  append = FALSE,
  delim = ",",
  quote_escape = "double",
  eol = "\n"
)

