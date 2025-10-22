print("==========================DATA PREP==================================")
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
#setwd("/home/mkaranja/Documents/BananaBreedingProject/Application/20250907/btract")

source("app/data_prep/fcts.R", local = T)
## ---------------------------
## 1. Data Loading & Preparation
## ---------------------------

# Load data using consistent path handling
data_path <- function(file) file.path("app", "data", file)

raw_banana <- readRDS(data_path("raw_banana.rds"))

accession_info <- readRDS("app/data/accession_details.rds") |>
  mutate(germplasmName = as.character(germplasmName))

## ---------------------------
##
## 2. Main Data Processing
## ---------------------------

# Function to create genotype links safely
create_genotype_link <- function(genotype, url) {
  case_when(
    is.na(genotype) | genotype == "" ~ NA_character_,
    is.na(url) | url == "" ~ genotype,
    TRUE ~ paste0('<a href="', url, '" target="_blank">', genotype, '</a>')
  )
}

# Process main banana data
banana <- raw_banana |>
  # Join female info
  left_join(
    accession_info |>
      select(
        Female_Genotype = germplasmName,
        Female_Ploidy = ploidy_level,
        Female_URL = url
      ),
    by = "Female_Genotype"
  ) |>
  # Join male info
  left_join(
    accession_info |>
      select(
        Male_Genotype = germplasmName,
        Male_Ploidy = ploidy_level,
        Male_URL = url
      ),
    by = "Male_Genotype"
  ) |>
  # Remove duplicates
  distinct(Crossnumber, .keep_all = TRUE) |>
  # Standardize cross type
  mutate(Cross_Type = stringr::str_to_title(gsub("multi-", "Multi-", Cross_Type))) |>
  # Handle ploidy data
  dplyr::mutate(
        Female_Ploidy_Level = na_if(Female_Ploidy_Level, ""),
        Female_Ploidy = na_if(Female_Ploidy, ""),
        Male_Ploidy_Level = na_if(Male_Ploidy_Level, ""),
        Male_Ploidy = na_if(Male_Ploidy, ""),
        Female_Ploidy_Level = tolower(coalesce(Female_Ploidy_Level, Female_Ploidy)),
        Male_Ploidy_Level = tolower(coalesce(Male_Ploidy_Level, Male_Ploidy))
      ) |>

    # Create clickable links
    mutate(
      Female_Genotype = create_genotype_link(Female_Genotype, Female_URL),
      Male_Genotype = create_genotype_link(Male_Genotype, Male_URL)
    ) |>
    # Select and order columns
    select(
      Location, Crossnumber, Cross_Type, Trial_Name,
      starts_with("Female_"), Cycle, starts_with("Male_"), Cross_Combination,
      First_Pollination_Date, Number_of_Repeats, Number_of_bracts,
      everything(),
      -c(Repeat_Pollination_Date, Female_Ploidy, Male_Ploidy, Female_URL, Male_URL)
    )

# Remove invalid crosses i.e crosses wwithout cycle no.
banana <- banana[!grepl("C/", banana$Crossnumber),]
banana <- banana |>
  dplyr::mutate_at(vars(Location, Cross_Type, Trial_Name, Female_Plot_Name, Female_Genotype, Female_Ploidy_Level, Cycle,
                        Male_Genotype, Male_Plot_Name, Male_Ploidy_Level, Cross_Combination), as.factor) |>
  dplyr::mutate_at(vars(Number_of_Repeats, Number_of_bracts, Total_Seeds, Good_Seeds, Number_of_Embryo_Rescued, Number_of_Embryo_Germinating), as.numeric) |>
  dplyr::mutate_at(vars(First_Pollination_Date, Bunch_Harvest_Date,Seed_Extraction_Date, Embryo_Rescue_Date, Germination_Date), as.Date)


banana2 <- raw_banana |>
  # Join female info
  left_join(
    accession_info |>
      select(
        Female_Genotype = germplasmName,
        Female_Ploidy = ploidy_level,
        Female_URL = url
      ),
    by = "Female_Genotype"
  ) |>
  # Join male info
  left_join(
    accession_info |>
      select(
        Male_Genotype = germplasmName,
        Male_Ploidy = ploidy_level,
        Male_URL = url
      ),
    by = "Male_Genotype"
  ) |>
  # Remove duplicates
  distinct(Crossnumber, .keep_all = TRUE) |>
  # Standardize cross type
  mutate(Cross_Type = stringr::str_to_title(gsub("multi-", "Multi-", Cross_Type))) |>
  # Handle ploidy data
  dplyr::mutate(
    Female_Ploidy_Level = na_if(Female_Ploidy_Level, ""),
    Female_Ploidy = na_if(Female_Ploidy, ""),
    Male_Ploidy_Level = na_if(Male_Ploidy_Level, ""),
    Male_Ploidy = na_if(Male_Ploidy, ""),
    Female_Ploidy_Level = tolower(coalesce(Female_Ploidy_Level, Female_Ploidy)),
    Male_Ploidy_Level = tolower(coalesce(Male_Ploidy_Level, Male_Ploidy))
  ) |>

  # Select and order columns
  select(
    Location, Crossnumber, Cross_Type, Trial_Name,
    starts_with("Female_"), Cycle, starts_with("Male_"), Cross_Combination,
    First_Pollination_Date, Number_of_Repeats, Number_of_bracts,
    everything(),
    -c(Repeat_Pollination_Date, Female_Ploidy, Male_Ploidy, Female_URL, Male_URL)
  )
# Remove invalid crosses i.e crosses wwithout cycle no.
banana2 <- banana2[!grepl("C/", banana$Crossnumber),]
banana2 <- banana2 |>
  dplyr::mutate_at(vars(Location, Cross_Type, Trial_Name, Female_Plot_Name, Female_Genotype, Female_Ploidy_Level, Cycle,
                        Male_Genotype, Male_Plot_Name, Male_Ploidy_Level, Cross_Combination), as.factor) |>
  dplyr::mutate_at(vars(Number_of_Repeats, Number_of_bracts, Total_Seeds, Good_Seeds, Number_of_Embryo_Rescued, Number_of_Embryo_Germinating), as.numeric) |>
  dplyr::mutate_at(vars(First_Pollination_Date, Bunch_Harvest_Date,Seed_Extraction_Date, Embryo_Rescue_Date, Germination_Date), as.Date)

# Save main dataset
process_data(
  df = banana,
  columns = NULL,
  data_type = "banana",
  filter_conditions = list("Crossnumber" = NULL)
)

## ---------------------------
## 3. Derived Data Processing
## ---------------------------
# Common columns for derived datasets
common_cols <- c("Location", "Crossnumber", "Female_Genotype", "Male_Genotype")
common_cols2 <- c("Location", "PlantletID", "Female_Genotype", "Male_Genotype")

# Flowering
flowering <- readRDS(data_path("raw_flowering.rds")) |>
  select(-Female_Genotype) |>
  left_join(banana |> select(Female_Plot_Name,Female_Genotype)) |>
  select("Location", "Female_Plot_Name", "Female_Genotype",  "Plant_Sex", "Flowering_Date")
saveRDS(flowering, data_path("all_flowering.rds"))

flowering |>
  dplyr::group_split(Location) |>
  purrr::walk(~ saveRDS(.x, data_path(paste0(tolower(unique(.x$Location)), "_", "flowering", ".rds"))))

# Plantlets
plantlets <- readRDS(data_path("raw_plantlets.rds")) |>
  select(-any_of(c("Female_Genotype", "Male_Genotype"))) |>
  # Join with main data
  left_join(banana |> select(Crossnumber,Female_Genotype, Male_Genotype)) # , Female_URL, Male_URL

process_data(
  df = plantlets,
  #columns = c("Location", "Female_Genotype", "Plant_Sex", "Flowering_Date"),
  data_type = "plantlets",
  filter_conditions = list("PlantletID" = NULL)
)

# Full dataset
dt1 <- readRDS(data_path(paste0("all_banana.rds")))
dt2  <- plantlets |>
  dplyr::group_by(Crossnumber) |>
  dplyr::summarise(
    Number_of_Subcultures = sum(na.omit(as.integer(Copies))),
    Number_Rooting = sum(na.omit(as.integer(Number_Rooting))),
    Number_Sent_Out = sum(na.omit(as.integer(Number_Sent_Out))),
    Weaning_2_Plantlets = sum(na.omit(as.integer(Weaning_2_Plantlets))),
    Number_in_Screenhouse = sum(na.omit(as.integer(Number_in_Screenhouse))),
    Number_in_hardening = sum(na.omit(as.integer(Number_in_hardening))),
    Number_in_Openfield = sum(na.omit(as.integer(Number_in_Openfield)))
  )

result <- dt1 |>
  dplyr::left_join(dt2) |>
  dplyr::filter(Crossnumber != '', Location != '') |> # Male_Genotype != '',
  #clean_genotype_columns() |> # remove url from genotypes
  dplyr::mutate(
    # Ensure data is properly filtered
    Banana_Bunches = ifelse(!is.na(Bunch_Harvest_Date), 1, 0),
    Year_of_Pollination = as.integer(lubridate::year(First_Pollination_Date)),
    Month_of_Pollination = lubridate::month(First_Pollination_Date)
  ) |>
  dplyr::mutate(
    Female_Ploidy = Female_Ploidy_Level,
    Male_Ploidy = Male_Ploidy_Level
  )

# Add and align month data
month_data <- data.frame(
  month = 1:12, x = LETTERS[1:12], Month_of_Pollination = 1:12
) |>
  dplyr::mutate(month = factor(month.name[month], levels = month.name)) |>
  dplyr::arrange(month)

result <- result |>
  dplyr::left_join(month_data, by = "Month_of_Pollination") |>
  dplyr::select(-c(x, Month_of_Pollination)) |>
  dplyr::rename(Month_of_Pollination = month)

# Select relevant columns
result <- result |>
  dplyr::select(
    Location, Crossnumber, Female_Genotype, Female_Ploidy, Male_Genotype,
    Male_Ploidy, Cross_Type, First_Pollination_Date, Year_of_Pollination,
    Month_of_Pollination, Number_of_Repeats, Banana_Bunches, Total_Seeds, Good_Seeds,
    Number_of_Embryo_Rescued, Number_of_Embryo_Germinating, Number_of_Subcultures,
    Number_Rooting, Number_Sent_Out, Weaning_2_Plantlets, Number_in_Screenhouse,
    Number_in_hardening, Number_in_Openfield
  )
# set data types
factor_cols <- c("Location", "Crossnumber", "Cross_Type", "Female_Genotype", "Female_Ploidy",
                 "Male_Genotype", "Male_Ploidy", "Year_of_Pollination", "Month_of_Pollination")
num_cols <- c(grep("Number", names(result), value = T), "Total_Seeds", "Good_Seeds", "Weaning_2_Plantlets" )

result <- result |>
  dplyr::mutate_at(dplyr::vars(dplyr::all_of(factor_cols)), as.factor) |>
  dplyr::mutate_at(dplyr::vars(dplyr::all_of(num_cols)), as.numeric)

# result |>
#     saveRDS(data_path("all_overall_banana.rds"))

process_data(
  df = result,
  columns = NULL,
  data_type = "overall_banana",
  filter_conditions = list("Crossnumber" = NULL)
)

# Split data by location
process_data(
  df = banana2,
  columns = NULL,
  data_type = "banana",
  filter_conditions = list("Crossnumber" = NULL)
)

# Crosses
process_data(
  df = banana2,
  columns = c(common_cols, "Cross_Type", "First_Pollination_Date"),
  data_type = "crosses",
  filter_conditions = list("First_Pollination_Date" = NA)
)

# Bunches
process_data(
  df = banana2,
  columns = c(common_cols, "Cross_Type", "Bunch_Harvest_Date"),
  data_type = "bunches",
  filter_conditions = list("Bunch_Harvest_Date" = NA)
)

# Seeds
process_data(
  df = banana2,
  columns = c(common_cols, "Cross_Type", "Seed_Extraction_Date", "Total_Seeds"),
  data_type = "extracted_seeds",
  filter_conditions = list(
    "Seed_Extraction_Date" = NA,
    "Total_Seeds" = 0
  )
)

# Embryo Rescue
process_data(
  df = banana2,
  columns = c(common_cols, "Cross_Type", "Embryo_Rescue_Date", "Number_of_Embryo_Rescued"),
  data_type = "embryo_rescue",
  filter_conditions = list(
    "Embryo_Rescue_Date" = NA,
    "Number_of_Embryo_Rescued" = 0
  )
)

# Germination
process_data(
  df = banana2,
  columns = c(common_cols, "Cross_Type", "Germination_Date", "Number_of_Embryo_Germinating"),
  data_type = "germination",
  filter_conditions = list(
    "Germination_Date" = NA,
    "Number_of_Embryo_Germinating" = 0
  )
)

# Germinating embryos ids
germination_ids = readRDS("app/data/raw_germination_ids.rds") |>
  dplyr::select(Location, Crossnumber, PlantletID, Germination_Date)
saveRDS(germination_ids, "all_germination_ids.rds")

# Save by location (if Location column exists)
if ("Location" %in% names(germination_ids)) {
  germination_ids |>
    dplyr::group_split(Location) |>
    purrr::walk(~ saveRDS(.x, data_path(paste0(tolower(unique(.x$Location)), "_germination_ids.rds"))))
} else {
  warning("Location column not found. Cannot split by location.")
}

# First, load all plantlets data
all_plantlets <- readRDS(data_path("all_plantlets.rds")) |>
  dplyr::left_join(
    readRDS(data_path("raw_ploidy.rds")) |>
      dplyr::select(
        PlantletID, Ploidy_Level, Ploidy_Analysis_Date
      )
  )

# Process direct germination data
#process_with_plantlets(
#  df = readRDS("app/data/raw_directgermination.rds"),
#  all_plantlets = all_plantlets,
#  join_columns = c("PlantletID", "Female_Genotype", "Male_Genotype"), # , "Female_URL", "Male_URL"
#  data_type = "directgermination"
#)

# Process subcultures data
process_with_plantlets(
  df = readRDS("app/data/raw_subcultures.rds"),
  all_plantlets = all_plantlets,
  join_columns = c("PlantletID", "Female_Genotype", "Male_Genotype"), # , "Female_URL", "Male_URL"
  data_type = "subcultures",
  filter_conditions = list("Copies" = 0)
)


# Process rooting data
process_with_plantlets(
  df = readRDS("app/data/raw_rooting.rds"),
  all_plantlets = all_plantlets,
  join_columns = c("PlantletID", "Female_Genotype", "Male_Genotype"), # "Female_URL", "Male_URL"
  data_type = "rooting",
  filter_conditions = list("Number_Rooting" = 0)
)

# Weaning 1
process_with_plantlets(
  df = readRDS(paste0("app/data/raw_weaning1.rds")),
  all_plantlets = all_plantlets,
  join_columns = c("PlantletID", "Female_Genotype", "Male_Genotype"),
  data_type = "weaning1",
  filter_conditions = list("Number_Sent_Out" = 0)
)

# Weaning 2
process_with_plantlets(
  df = readRDS(paste0("app/data/raw_weaning2.rds")),
  all_plantlets = all_plantlets,
  join_columns = c("PlantletID", "Female_Genotype", "Male_Genotype"),
  data_type = "weaning2",
  filter_conditions = list("Weaning_2_Plantlets" = 0)
)

# Screenhouse
process_with_plantlets(
  df = readRDS(paste0("app/data/raw_screenhouse.rds")),
  all_plantlets = all_plantlets,
  join_columns = c("PlantletID", "Female_Genotype", "Male_Genotype"),
  data_type = "screenhouse",
  filter_conditions = list("Number_in_Screenhouse" = 0)
)


# Hardening
process_with_plantlets(
  df = readRDS(paste0("app/data/raw_hardening.rds")),
  all_plantlets = all_plantlets,
  join_columns = c("PlantletID", "Female_Genotype", "Male_Genotype"),
  data_type = "hardening",
  filter_conditions = list("Number_in_hardening" = 0)
)

# Openfield
process_with_plantlets(
  df = readRDS(paste0("app/data/raw_openfield.rds")),
  all_plantlets = all_plantlets,
  join_columns = c("PlantletID", "Female_Genotype", "Male_Genotype"),
  data_type = "openfield",
  filter_conditions = list("Number_in_Openfield" = 0)
)

# contamination
process_with_plantlets(
  df = readRDS(paste0("app/data/raw_contamination.rds")),
  all_plantlets = all_plantlets,
  join_columns = c("PlantletID", "Female_Genotype", "Male_Genotype"),
  data_type = "contamination",
  filter_conditions = list("contaminated" = 0)
)


