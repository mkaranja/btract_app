print("1. Load libraries-----------------------------------------")
library(dplyr)
library(lubridate)
library(data.table)
library(magrittr)
library(lubridate)
library(stringr)
library(purrr) # For map_df function
library(brapi)

print("2. Set working dir ---------------------------")
setwd("/srv/shiny-server/btract/btract")

source("app/data_prep/fcts.R", local = T)
# source("app/data_prep/data_fcts.R")

# 1. Legacy data - run once
# source("app/data_prep/step1_legacy_data.R")

# 2. Raw data from ONA
print("3. Process raw data --------------------------")
source("app/data_prep/step2_raw_data_prep.R", local = T)

# 3. Get accessions ploidy levels and links from Musabase
print("4. Get accessions details ---------------------------")
# source("app/data_prep/step3_accessions_details.R", local = T)

# 4. Prep-data for the dashboard
print("5. Save data ---------------------------")
source("app/data_prep/step4_prep_data.R", local = T)

print("End of script")
