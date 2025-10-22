
box::use(
  shiny[reactive],
  lubridate[make_date, year, years]
)

banana <- readRDS(paste0("app/data/all_banana.rds"))

#' @export
sites <- levels(banana$Location)

# Subtract 3 years and set the date to January 1st
#' @export
last_3_years <- make_date(year(Sys.Date() - years(2)), 1, 1)

#' @export
start_date <- min(lubridate::ymd(banana$First_Pollination_Date), na.rm=T)

#' @export
end_date <- max(lubridate::ymd(banana$First_Pollination_Date), na.rm=T)

#' @export
r <- function(stage = NULL) {
  # Common columns shared across stages
  common_cols <- c("Location", "Crossnumber", "Cross_Type", "Female_Genotype",
                   "Female_Ploidy", "Male_Ploidy", "Female_Sub_Group",
                   "Male_Genotype", "Male_Sub_Group")

  # Define a lookup table for stage configurations
  stage_config <- list(
    "Crosses" = list(
      additional_cols = c("First_Pollination_Date"),
      date_col = "First_Pollination_Date",
      num_col = NULL
    ),
    "Banana Bunches" = list(
      additional_cols = c("Bunch_Harvest_Date"),
      date_col = "Bunch_Harvest_Date",
      num_col = NULL
    ),
    "Seeds extracted" = list(
      additional_cols = c("Seed_Extraction_Date", "Total_Seeds", "Good_Seeds"),
      date_col = "Seed_Extraction_Date",
      num_col = "Total_Seeds"
    ),
    "Embryo rescued" = list(
      additional_cols = c("Embryo_Rescue_Date", "Number_of_Embryo_Rescued"),
      date_col = "Embryo_Rescue_Date",
      num_col = "Number_of_Embryo_Rescued"
    ),
    "Embryo germination" = list(
      additional_cols = c("Germination_Date", "Number_of_Embryo_Germinating"),
      date_col = "Germination_Date",
      num_col = "Number_of_Embryo_Germinating"
    ),
    "Default" = list(
      additional_cols = c("Number_in_Openfield", "Openfield_Transfer_Date"),
      date_col = "Openfield_Transfer_Date",
      num_col = "Number_in_Openfield"
    )
  )

  # Use the provided stage or default to "Default"
  stage <- ifelse(!is.null(stage) && stage %in% names(stage_config), stage, "Default")

  # Retrieve the configuration for the selected stage
  config <- stage_config[[stage]]

  # Combine common columns with the activity-specific additional columns
  cols <- c(common_cols, config$additional_cols)

  # Return the result as a list
  return(list(cols = cols, date_col = config$date_col, num_col = config$num_col))
}



#' @export
activity_table_cols <- function(activity) {
  # Common columns shared across activities
  common_cols <- c("Location", "Crossnumber", "Cross_Type", "Female_Genotype",
                   "Female_Ploidy", "Male_Ploidy", "Female_Sub_Group",
                   "Male_Genotype", "Male_Sub_Group")

  # Define activity-specific additions and conditions
  activity_config <- list(
    first_pollination = list(
      additional_cols = c("First_Pollination_Date"),
      sub_ = c("Crossnumber", "First_Pollination_Date")
    ),
    banana_bunches = list(
      additional_cols = c("Bunch_Harvest_Date"),
      sub_ = c("Crossnumber", "Bunch_Harvest_Date")
    ),
    seed_extraction = list(
      additional_cols = c("Seed_Extraction_Date", "Total_Seeds", "Good_Seeds"),
      sub_ = c("Crossnumber", "Seed_Extraction_Date")
    ),
    embryo_rescue = list(
      additional_cols = c("Embryo_Rescue_Date", "Number_of_Embryo_Rescued"),
      sub_ = c("Crossnumber", "Bunch_Harvest_Date")
    ),
    embryo_germination = list(
      additional_cols = c("Germination_Date", "Number_of_Embryo_Germinating"),
      sub_ = c("Crossnumber", "Bunch_Harvest_Date")
    ),
    sub = list(
      additional_cols = c("PlantletID", "Copies", "Subculture_Date"),
      sub_ = c("PlantletID", "Subculture_Date")
    ),
    root = list(
      additional_cols = c("PlantletID", "Date_of_Rooting", "Number_Rooting"),
      sub_ = c("PlantletID", "Date_of_Rooting")
    )
  )

  # Check if activity exists in the configuration
  if (!activity %in% names(activity_config)) {
    stop("Invalid activity: ", activity)
  }

  # Get the specific configuration for the activity
  config <- activity_config[[activity]]

  # Combine common columns with activity-specific columns
  cols <- c(common_cols, config$additional_cols)
  sub_ <- config$sub_
  date_col <- grep("Date", cols, value = TRUE)


  if(length(grep("Number_", cols))){
    seed_col <- grep("Number_", cols, value = TRUE)
  } else if(length(grep("Total_Seeds", cols))){
    seed_col <- c(grep("Total_Seeds", cols, value = TRUE), grep("Good_Seeds", cols, value = TRUE))
  } else {
    seed_col <- ""
  }

  # Return the result as a list
  return(list(cols, sub_, date_col, seed_col))
}


cols <- activity_table_cols("seed_extraction")
cols
