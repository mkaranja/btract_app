library(dplyr)
library(lubridate)
library(data.table)
library(magrittr)
library(lubridate)
library(stringr)
library(purrr) # For map_df function
library(brapi)


print("==========================LEGACY DATA==================================")
setwd("/srv/shiny-server/btract/btract")
source("app/data_prep/fcts.R", local = T)

# Legacy data
#path <- "/home/mkaranja/Documents/BananaBreedingProject/Application/2025/data"
path <- "/home/mwk66/BTRACT/daily/data"
# Arusha
# 2015
# Read and process the data
arusha_crosses_2015 <- readRDS(paste0(path, "/Last_6_months_tisssue_culture_data.rds")) |>
  # Remove duplicates and unwanted columns
  unique() |>
  select(-c(contamination, badseeds, contains("days"))) |>
  # Rename columns using a more maintainable approach
  rename_with(~ c(
    "Location", "Crossnumber", "Female_Genotype", "Male_Genotype",
    "First_Pollination_Date", "Bunch_Harvest_Date", "Seed_Extraction_Date",
    "Total_Seeds", "Good_Seeds", "Number_of_Embryo_Rescued",
    "Embryo_Rescue_Date", "Germination_Date", "Number_of_Embryo_Germinating"
  )) |>
  # Add Cross_Type column with default value
  mutate(Cross_Type = "Bi-parental",
         Cross_Combination = paste0(Female_Genotype,"/", Male_Genotype))

# 2012 to 2017
arusha_crosses_2012_2017 <- data.table::fread(paste0(path, "/Arusha_crosses_2012_2017_in_musabase.csv"))[,-c(1,9,12,14,18:21)] |>
  rename_with(~ c(
  "Crossnumber", "Cross_Combination", "Cross_Type", "Female_Genotype", "Male_Genotype",
  "First_Pollination_Date", "Bunch_Harvest_Date", "Seed_Extraction_Date",
  "Total_Seeds", "Good_Seeds", "Number_of_Embryo_Rescued",
  "Embryo_Rescue_Date", "Number_of_Embryo_Germinating")) |>
  mutate(
    across(c(Total_Seeds, Good_Seeds, Number_of_Embryo_Rescued,
                  Number_of_Embryo_Germinating), as.integer),
    across(ends_with("Date"),
           ~ as.Date(lubridate::parse_date_time(., orders = c("mdy", "dmy", "ymd"))))
  )


arusha_crosses_2012_2017$Location = "Arusha"
arusha_crosses_2012_2017$Cross_Type <- "Bi-Parental"
arusha_crosses_2012_2017$Germination_Date = lubridate::ymd(arusha_crosses_2012_2017$Embryo_Rescue_Date) + 14 # germination date after 2 weeks

# Most of 2015 data is in 2012-2017
# data no in 2012 - 2017
arusha_2015_not_in_2012_2017 <- arusha_crosses_2015[!arusha_crosses_2015$Crossnumber %in% arusha_crosses_2012_2017$Crossnumber,]

arusha_data <- plyr::rbind.fill(arusha_2015_not_in_2012_2017, arusha_crosses_2012_2017) |>
  dplyr::filter(!is.na(First_Pollination_Date))

# Ibadan
ibadan_crosses <- readRDS(paste0(path,"/ibadan_pollination_data_june2024.rds")) |>
  dplyr::filter(!is.na(First_Pollination_Date)) |>
  mutate(Cross_Type = "Bi-Parental",
         Cross_Combination = paste0(Female_Genotype,"/", Male_Genotype))

# # combine all data and save
plyr::rbind.fill(arusha_data, ibadan_crosses) |>
  dplyr::filter(!is.na(First_Pollination_Date)) |>
  unique() |>
  saveRDS("app/data/btract_legacy_data.rds")
