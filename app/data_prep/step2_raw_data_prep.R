print("==========================ONA DATA==================================")
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

# setwd("/srv/shiny-server/btract/btract")

# READ CURRENT DATA
data_dir <- "/home/mwk66/BTRACT/daily/data"

current_banana <- read_agri_dataset(
  data_dir = data_dir,
  dataset_type = "banana",
  col_specs = list(
    Location = list(type = "factor"),  # Auto-detected levels
    Crossnumber = list(type = "character"),
    Mother = list(new_name = "Female_Genotype", type = "character"),
    Father = list(new_name = "Male_Genotype", type = "character"),
    TrialName = list(new_name = "Trial_Name", type = "character"),
    FemalePlotName = list(new_name = "Female_Plot_Name", type = "character"),
    MalePlotName = list(new_name = "Male_Plot_Name", type = "character"),
    FemalePloidyLevel = list(new_name = "Female_Ploidy_Level", type = "character"),
    MalePloidyLevel = list(new_name = "Male_Ploidy_Level", type = "character"),
    CrossType = list(new_name = "Cross_Type", type = "factor"),
    First_Pollination_Date = list(type = "Date", origin = "1970-01-01"),
    Number_of_Repeats = list(type = "numeric"),
    Bunch_Harvest_Date = list(type = "Date", origin = "1970-01-01"),
    Seed_Extraction_Date = list(type = "Date", origin = "1970-01-01"),
    Total_Seeds = list(type = "numeric"),
    Good_Seeds = list(type = "numeric"),
    Embryo_Rescue_Date = list(type = "Date", origin = "1970-01-01"),
    Number_of_Embryo_Rescued = list(type = "numeric"),
    Number_of_Embryo_Germinating = list(type = "numeric"),
    Germination_Date = list(type = "Date", origin = "1970-01-01")
  ),
  file_pattern = "BananaData",
  id_var = "Crossnumber"
  )

current_banana$Cross_Type <- ifelse(current_banana$Cross_Type == "Bi-parental", "Bi-Parental",
                                    ifelse(current_banana$Cross_Type == "multi-parental", "Multi-Parental", current_banana$Cross_Type))
# crosses type
current_banana$Cross_Type <- with(current_banana, {
  is_backcross <- substr(Female_Genotype, 1, 2) %in% c("T.", "NM") |
    substr(Male_Genotype, 1, 2) %in% c("T.", "NM")
  ifelse(is_backcross, "Back-Cross", ifelse(is.na(Cross_Type), "Bi-Parental", Cross_Type))
})


# STEP 2: MERGE WITH LEGACY DATA
legacy_data <- readRDS("app/data/btract_legacy_data.rds")

banana <- plyr::rbind.fill(current_banana, legacy_data) |>
  dplyr::select(Location, Crossnumber, Cross_Type, everything())

#banana_crosses[,c("Number_of_bracts", "Female_Ploidy_Level","Male_Ploidy_Level")] <- NULL

# STANDARDIZE NAMES OF BOTH Female_Genotype & Male_Genotype
banana$Female_Genotype <- standardize_genotypes(banana$Female_Genotype)
banana$Male_Genotype <- standardize_genotypes(banana$Male_Genotype)

banana |>
  saveRDS("app/data/raw_banana.rds")


# Other datasets

# Flowering
read_agri_dataset(
  data_dir = data_dir,
  dataset_type = "flowering",
  col_specs = list(
    Location = list(type = "factor"),  # Auto-detected levels
    FlowerID = list(new_name = "Female_Plot_Name", type = "character"),
    Genotype = list(new_name = "Female_Genotype", type = "character"),
    Flowering_Date = list(type = "Date", origin = "1970-01-01")
  ),
  file_pattern = "AllFlowering",
  id_var = "Female_Plot_Name"
) |>
  saveRDS("app/data/raw_flowering.rds")


# Ploidy
ploidy <- read_agri_dataset(
  data_dir = data_dir,
  dataset_type = "ploidy",
  col_specs = list(
    PlantletID = list(type = "character"),
    Ploidy_Analysis_Date = list(type = "Date", origin = "1970-01-01"),
    Ploidy_Level = list(type = "factor")
  ),
  file_pattern = "Ploidy",
  id_var = "PlantletID"
)
ploidy |>
  saveRDS("app/data/raw_ploidy.rds")

# plantlets
read_agri_dataset(
  data_dir = data_dir,
  dataset_type = "plantlets",
  col_specs = list(
    Location = list(type = "factor"),  # Auto-detected levels
    Crossnumber = list(type = "character"),  # Auto-detected levels
    Mother = list(type = "character"),  # Auto-detected levels
    Father = list(type = "character"),  # Auto-detected levels
    PlantletID = list(type = "character"),
    Ploidy_Analysis_Date = list(type = "Date", origin = "1970-01-01"),
    Ploidy_Level= list(type = "factor"),
    Germination_Submission_Date = list(type = "Date", origin = "1970-01-01"),
    Germination_Date = list(type = "Date", origin = "1970-01-01"),
    Copies = list(type = "numeric"),
    Number_Rooting = list(type = "numeric"),
    Number_Sent_Out = list(type = "numeric"),
    Weaning_2_Plantlets = list(type = "numeric"),
    Number_in_Screenhouse = list(type = "numeric"),
    Number_in_hardening = list(type = "numeric"),
    Number_in_Openfield = list(type = "numeric")
  ),
  file_pattern = "PlantletsData",
  id_var = "PlantletID"
) |>
  dplyr::left_join(ploidy) |>
  saveRDS("app/data/raw_plantlets.rds")

# Subcultures
read_agri_dataset(
  data_dir = data_dir,
  dataset_type = "subcultures",
  col_specs = list(
    Location = list(type = "factor"),  # Auto-detected levels
    PlantletID = list(type = "character"),
    Subculture_Date = list(type = "Date", origin = "1970-01-01"),
    Copies = list(type = "numeric")
  ),
  file_pattern = "Subcultures",
  id_var = "PlantletID"
) |>
  dplyr::left_join(ploidy) |>
  saveRDS("app/data/raw_subcultures.rds")

# Rooting
read_agri_dataset_r2(
  data_dir = data_dir,
  dataset_type = "rooting",
  col_specs = list(
    Location = list(type = "factor"),  # Auto-detected levels
    PlantletID = list(type = "character"),
    Number = list(new_name = "Number_rooting",type = "numeric"),
    Date_of_rooting = list(type = "Date", origin = "1970-01-01")
  ),
  file_pattern = "Rooting",
  id_var = "PlantletID"
  ) |>
  dplyr::left_join(ploidy) |>
  saveRDS("app/data/raw_rooting.rds")

# Weaning1
read_agri_dataset_r2(
  data_dir = data_dir,
  dataset_type = "weaning1",
  col_specs = list(
    Location = list(type = "factor"),  # Auto-detected levels
    PlantletID = list(type = "character"),
    Sent_Out_Date = list(type = "Date", origin = "1970-01-01"),
    Number_Sent_Out = list(type = "numeric")
  ),
  file_pattern = "Weaning1",
  id_var = "PlantletID"
  ) |>
  dplyr::left_join(ploidy) |>
  saveRDS("app/data/raw_weaning1.rds")

#
read_agri_dataset_r2(
  data_dir = data_dir,
  dataset_type = "weaning2",
  col_specs = list(
    Location = list(type = "factor"),  # Auto-detected levels
    PlantletID = list(type = "character"),
    Weaning_2_Date = list(type = "Date", origin = "1970-01-01"),
    Number = list(new_name = "Weaning_2_Plantlets", type = "numeric")
  ),
  file_pattern = "Weaning2",
  id_var = "PlantletID") |>
  dplyr::left_join(ploidy) |>
  saveRDS("app/data/raw_weaning2.rds")

#
read_agri_dataset_r2(
  data_dir = data_dir,
  dataset_type = "screenhouse",
  col_specs = list(
    Location = list(type = "factor"),  # Auto-detected levels
    PlantletID = list(type = "character"),
    Screenhouse_Transfer_Date = list(type = "Date", origin = "1970-01-01"),
    Number_in_Screenhouse = list(type = "numeric")
  ),
  file_pattern = "Screenhouse",
  id_var = "PlantletID") |>
  dplyr::left_join(ploidy) |>
  saveRDS("app/data/raw_screenhouse.rds")

#
read_agri_dataset_r2(
  data_dir = data_dir,
  dataset_type = "hardening",
  col_specs = list(
    Location = list(type = "factor"),  # Auto-detected levels
    PlantletID = list(type = "character"),
    Hardening_Date = list(type = "Date", origin = "1970-01-01"),
    Number = list(new_name = "Number_in_hardening", type = "numeric")
  ),
  file_pattern = "Hardening",
  id_var = "PlantletID") |>
  dplyr::left_join(ploidy) |>
  saveRDS("app/data/raw_hardening.rds")

#
read_agri_dataset_r2(
  data_dir = data_dir,
  dataset_type = "openfield",
  col_specs = list(
    Location = list(type = "factor"),  # Auto-detected levels
    PlantletID = list(type = "character"),
    Openfield_Transfer_Date = list(type = "Date", origin = "1970-01-01"),
    Number = list(new_name = "Number_in_Openfield", type = "numeric")
  ),
  file_pattern = "Openfield",
  id_var = "PlantletID") |>
  dplyr::left_join(ploidy) |>
  saveRDS("app/data/raw_openfield.rds")

read_agri_dataset_r2(
  data_dir = data_dir,
  dataset_type = "germination_ids",
  col_specs = list(
    Location = list(type = "factor"),  # Auto-detected levels
    Crossnumber = list(type = "character"),
    PlantletID = list(type = "character"),
    Germination_Date = list(type = "Date", origin = "1970-01-01"),
    Germination_Submission_Date = list(type = "Date", origin = "1970-01-01")
  ),
  file_pattern = "GerminatingIDs",
  id_var = "PlantletID") |>
  saveRDS("app/data/raw_germination_ids.rds")


# Direct Germination
#read_agri_dataset_r(
#  data_dir = data_dir,
#  dataset_type = "directgermination",
#  col_specs = list(
#    Location = list(type = "factor"),  # Auto-detected levels
#    PlantletID = list(type = "character"),
#    Germination_Date = list(type = "Date", origin = "1970-01-01")
#  ),
#  file_pattern = "DirectGermination",
#  id_var = "PlantletID"
#) |>
#  dplyr::left_join(ploidy) |>
#  saveRDS("app/data/raw_directgermination.rds")

read_agri_dataset_r2(
  data_dir = data_dir,
  dataset_type = "Contamination",
  col_specs = list(
    Location = list(type = "factor"),  # Auto-detected levels
    PlantletID = list(type = "character"),
    Contamination_Date = list(type = "Date", origin = "1970-01-01"),
    contaminated = list(type = "integer")
  ),
  file_pattern = "Contamination",
  id_var = "PlantletID") |>
  saveRDS("app/data/raw_contamination.rds")

