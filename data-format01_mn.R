#' ---
#' title: MN data formatting
#' author: Akira Terui
#' output:
#'   html_document:
#'     theme: paper
#'     toc: true
#'     toc_float: true
#' ---

#' # Session infomation
  
  sessionInfo()

#' # Load library

  # Load library
  rm(list=ls(all.names=T))
  library(tidyverse)

#' # Species removal information
#' This information is included for comparison betwee Terui's and Kim's script
  
  # formatting information --------------------------------------------------
  ## species name used in Kim's original script for removal
  ## this code section is created to just compare Terui's and Kim's scripts
  sp_rm <- c("AMEIURUS_SPP.","CAMPOSTOMA_SPP.","CARPIODES_SPP.","CATOSTOMIDAE_-_UNIDENTIFIED",
             "COMMON_SHINER_X_CREEK_CHUB","COMMON_SHINER_X_EMERALD_SHINER","COMMON_SHINER_X_HORNYHEAD",
             "COMMON_SHINER_X_SOUTHERN_REDBELLY_DACE","COMMON_SHINER_X_STONEROLLER","COMMON_SHINER_X_UNKNOWN",
             "COMMON_SHINER_X_HORNYHEAD","COTTUS_SPP.","CREEK_CHUB_X_HORNYHEAD_CHUB","CREEK_CHUB_X_UNKNOWN",
             "CYPRINELLA_SPP.","CYPRINIDAE_-_UNIDENTIFIED","FAM:_LAMPREY","FAM:_MINNOWS","GEN:_BUFFALOS",
             "GEN:_CARPSUCKERS","GEN:_CATOSTOMUS","GEN:_COMMON_SUNFISHES","GEN:_ETHEOSTOMA","GEN:_NOTROPIS",
             "GEN:_PERCINA","GEN:_PHOXINUS","GEN:_REDHORSES","GEN:_STONEROLLERS","GREEN_SUNFISH_X_BLUEGILL",
             "GREEN_SUNFISH_X_BLUEGILL_X_PUMPKINSEED","GREEN_SUNFISH_X_ORANGESPOTTED_SUNFISH",
             "GREEN_SUNFISH_X_PUMPKINSEED","HIODON_SPP.","HYBOGNATHUS_SPP.","HYBRID:_COMMON_SHINER_X_SO._REDBELLY_DACE",
             "HYBRID:_GREEN_SUNF._X_BLUEGILL","HYBRID_MINNOW","HYBRID_PHOXINUS","HYBRID_SUNFISH","ICTIOBUS_SPP.",
             "LAMPREY_AMMOCOETE","LAMPREYS_(AMMOCOETE)","LAMPREYS","LEPISOSTEUS_SPP.","MOXOSTOMA_SPP.",
             "NO_FISH_CAPTURED","NOTROPIS_SPP.","PERCINA_SPP.","POMOXIS_SPP.","PUMPKINSEED_X_BLUEGILL",
             "PUMPKINSEED_X_WARMOUTH","ROSYFACE/CARMINE_SHINER","RUSTY_CRAYFISH","STICKLEBACKS_UNSP.",
             "SUBFAM:_BUFFALO/CARPSUCKERS","TIGER_TROUT","TIGER_TROUT_(I21_X_I22)", "STONEROLLERS","PEARL_DACE",
             "QUILLBACK_CARPSUCKER", "GOLDFISH")
  
  ## format name
  #MN.FishData[MN.FishData=="AMERICAN_BROOK_LAMPREY_(AMMOCOETE)"]<- "AMERICAN_BROOK_LAMPREY"
  #MN.FishData[MN.FishData=="CHESNUT_LAMPREY_(AMMOCOETE)"]<- "CHESTNUT_LAMPREY"
  #MN.FishData[MN.FishData=="NORTHERN_BROOK_LAMPREY_(AMMOCOETE)"]<- "NORTHERN_BROOK_LAMPREY"
  #MN.FishData[MN.FishData=="SILVER_LAMPREY_(AMMOCOETE)"]<- "SILVER_LAMPREY"
  #MN.FishData[MN.FishData=="SOUTHERN_BROOK_LAMPREY_(AMMOCOETE)"]<- "SOUTHERN_BROOK_LAMPREY"
  #MN.FishData[MN.FishData=="NORTHERN_HOG_SUCKER"]<- "NORTHERN_HOGSUCKER"
  #MN.FishData[MN.FishData=="TROUT-PERCH"]<- "TROUT_PERCH"
  #MN.FishData[MN.FishData=="TROUTPERCH"]<- "TROUT_PERCH"

  

#' # Read data
#' **NOTE: use `readr::read_csv()` instead of `read.csv()` to read data - `read.csv()` treated blanks weiredly in this dataset**
#' 
#' - `01_data/org-data_mn_visit.csv` - visit data
#' - `01_data/org-data_mn_station.csv` - site date
#' - `01_data/org-data_mn_fish.csv` - fish data
#+ message = FALSE, warning = FALSE
  
  # Read data ---------------------------------------------------------------
  dat_visit <- read_csv("01_data/org-data_mn_visit.csv") %>% mutate(FieldNum = str_to_upper(FieldNum) )
  dat_site <- read_csv("01_data/org-data_mn_station.csv") %>% mutate(FieldNum = str_to_upper(FieldNum) )
  dat_fish <- read_csv("01_data/org-data_mn_fish.csv") %>% mutate(FieldNum = str_to_upper(FieldNum) )

#' # Fish data formatting
#' Remove unidentified or hybrid species. These are identified if `Name2` is either `NA`, `hybrid` or `larvae`

  # Fish data formatting ----------------------------------------------------
  ## Check unidentified or hybrid species
  unique(dat_fish$CommonName[is.na(dat_fish$Name2)])
  unique(dat_fish$CommonName[dat_fish$Name2 %in% c("hybrid", "larvae")])

  ## format CommonName
  ## replace space with underscore
  ## rename to upppercases
  ## remove '_(AMMOCOETE)' from string
  ## replace 'NORTHERN_HOG_SUCKER' with 'NORTHERN_HOGSUCKER'
  ## replace 'TROUT-PERCH' and 'TROUTPERCH' with 'TROUT_PERCH'
  ## drop unidentified
  ## drop hybrid and larval species
  ## remove GOLDFISH and PEARL DACE
  
  dat_fish %>%
    mutate(CommonName = str_replace(CommonName, pattern = " ", replacement = "_") ) %>%
    mutate(CommonName = str_to_upper(CommonName) ) %>%
    mutate(CommonName = str_replace(CommonName, "_\\(AMMOCOETE\\)", "")) %>% # remove '_(AMMOCOETE)' from string
    mutate(CommonName = str_replace(CommonName, "NORTHERN_HOG_SUCKER", "NORTHERN_HOGSUCKER")) %>%
    mutate(CommonName = str_replace(CommonName, "TROUT-PERCH", "TROUT_PERCH")) %>%
    mutate(CommonName = str_replace(CommonName, "TROUTPERCH", "TROUT_PERCH")) %>%
    drop_na(Name2) %>% # remove NA
    filter(!(Name2 %in% c("hybrid", "larvae")) ) %>% # remove c("hybrid", "larvae")
    filter(!(CommonName %in% c("GOLDFISH", "PEARL_DACE")) ) -> dat_fish # remove c("GOLDFISH", "PEARL_DACE")
  
  ## check difference from Kim's script - why pearl dace removed?
  ## one observation fewer than Kim's original file
  print(unique(dat_fish$CommonName[dat_fish$CommonName %in% sp_rm]) )

#' # Merge datasets
#' Merge `dat_fish`, `dat_visit` and `dat_site`
#' 
#' - `dat_fish` as a base dataset because it includes fish data
#' - `dat_visit` is joined to obtain sampling date
#' - `dat_site` is joined to obtain GPS location (Lat and Lon)

  # Add visit and station data to fish data ---------------------------------
  dat = dat_fish %>%
    left_join(dat_visit, by = "VisitNum") %>% # merge Visit data
    rename(FieldNum = FieldNum.x) %>% # rename FieldNum (derive from fish_dat)
    left_join(dat_site, by = "FieldNum") %>% # merge site information
    drop_na(FieldNum.y) %>% # drop NA FieldNum in dat_visit
    select(c(VisitNum, FieldNum, Name1, Name2, CommonName,
             VisitDate, StationLength, Number, LAT8xDD, LON8xDD) ) %>% # select columns
    rename(Genus = Name1, Species = Name2, Count = Number, Lat = LAT8xDD, Lon = LON8xDD) # rename columns
  
  length(unique(dat$VisitNum) ) # n = 3049
  length(unique(dat$FieldNum) ) # n = 2372
                