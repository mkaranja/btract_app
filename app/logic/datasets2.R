
box::use(
  shiny[reactive],
  data.table[fread,setDT, data.table, setnames, tstrsplit],
  lubridate[year, month, day]
)

#' @export
filter_data <- function(dataset, start, end, site) {
  # Load the data as a data.table
  dt <- readRDS(paste0("./app/data/", dataset, ".rds"))

  # Filter by site
  dt <- dt[Location == site]

  # Find the date column
  date_col <- grep("Date", names(dt), value = TRUE)[1]

  # Filter by date range using the date column dynamically
  dt <- dt[get(date_col) >= start & get(date_col) <= end]

  return(dt)
}



#' @export
load_data <- function(data_file, cols = NULL) {
  file_path <- paste0("./app/data/", data_file, ".rds")

  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }

  # Load the data as a data.table
  data <- readRDS(file_path)
  setDT(data)  # Convert to data.table if not already

  # Remove duplicate rows
  data <- unique(data)

  # Select specified columns if provided
  if (!is.null(cols)) {
    data <- data[, ..cols]
  }
  return(data)
}



#' @export
load_openfield <- function(data_file1, data_file2) {
  file_path1 <- paste0("./app/data/", data_file1, ".rds")
  file_path2 <- paste0("./app/data/", data_file2, ".rds")

  if (!file.exists(file_path1)) {
    stop("File does not exist: ", file_path1)
  }

  if (!file.exists(file_path2)) {
    stop("File does not exist: ", file_path2)
  }

  # Load and process the first dataset
  banana <- readRDS(file_path1)
  setDT(banana)  # Convert to data.table
  banana <- unique(banana)
  dt1 <- banana[, .(Location, Crossnumber, Cross_Type, Female_Genotype, Male_Genotype, First_Pollination_Date)]

  # Load and process the second dataset
  openfield <- readRDS(file_path2)
  setDT(openfield)  # Convert to data.table
  openfield <- unique(openfield)
  dt2 <- openfield[, .(Location, Crossnumber, PlantletID, Female_Genotype, Male_Genotype, Openfield_Transfer_Date, Number_in_Openfield)]

  # Perform a left join
  data <- dt2[dt1, on = .(Location, Crossnumber, Female_Genotype, Male_Genotype)]

  return(data)
}


#' @export
highchart_data <- function(data, stage, cols, date_col, num_col = NULL, start_date, end_date, site = NULL) {
  # Select and rename columns
  data <- setDT(data)[, .SD, .SDcols = cols]
  setnames(data, date_col, "Date")

  # Filter by site if specified
  if (!is.null(site) && site != "All") {
    data <- data[Location %in% site]
  }

  # Filter by date range
  data <- data[Date >= start_date & Date <= end_date]

  # Add date components
  data[, `:=`(
    Yearly = year(Date),
    Monthly = month(Date),
    Daily = day(Date)
  )]

  # Determine the "Number" column
  if (stage %in% c("Crosses", "Banana Bunches")) {
    data[, Number := 1]
  } else {
    data <- data[!is.na(get(num_col))]
    data[, Number := get(num_col)]
  }

  return(data)
}


#' @export
grouped_data <- function(data, group) {
  setDT(data)  # Ensure the data is a data.table

  if (group == "daily") {
    # Group by Location and Date
    data <- data[, .(number = sum(Number, na.rm = TRUE)), by = .(Location, Date)]
    data[, Time := Date]
    group_name <- "Daily"
  } else if (group == "monthly") {
    # Create a Time column in "Year-Month" format and group by Location and Time
    data[, Time := sprintf("%d-%02d", Yearly, Monthly)]
    data <- data[, .(number = sum(Number, na.rm = TRUE)), by = .(Location, Time)]
    group_name <- "Monthly"
  } else if (group == "yearly") {
    # Group by Location and Yearly
    data <- data[, .(number = sum(Number, na.rm = TRUE)), by = .(Location, Yearly)]
    data[, Time := Yearly]
    group_name <- "Yearly"
  }

  return(data)
}



#' @export
load_activity_data <- function(data_file, cols = NULL, sub_ = NULL, date_col, start_, end_) {
  file_path <- paste0("./app/data/", data_file, ".rds")

  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }

  # Load and ensure unique data
  data <- readRDS(file_path)
  setDT(data)
  data <- unique(data)

  # Select specific columns if provided
  if (!is.null(cols)) {
    data <- data[, ..cols]
  }

  # Filter rows where all specified sub_ columns are not NA
  if (!is.null(sub_)) {
    data <- data[!Reduce(`|`, lapply(.SD, is.na)), .SDcols = sub_]
  }

  # Filter by date range
  data <- data[get(date_col) >= start_ & get(date_col) <= end_]

  return(data)
}




# TC labels for download
#' @export
tc_embryo_data <- function(dt, plant_id, embryo_col, number_per_tube, number_of_copies) {
  # Ensure dt is a data.table
  if (!"data.table" %in% class(dt)) {
    setDT(dt)
  }

  # Filter rows if plant_id is provided
  if (!is.null(plant_id)) {
    dt <- dt[PlantletID %in% plant_id]
  }

  # Replicate rows if number_of_copies > 0
  if (number_of_copies > 0) {
    dt <- dt[rep(seq_len(nrow(dt)), each = number_of_copies)]
  }

  # Split PlantletID into Prefix, Suffix, and EmbryoNo
  split_plantlet <- tstrsplit(as.character(dt$PlantletID), "_", fixed = TRUE)
  dt[, c("Prefix", "Suffix", "EmbryoNo") := .(split_plantlet[[1]], split_plantlet[[2]], split_plantlet[[3]])]

  # Clean up Suffix by removing parentheses and their contents
  dt[, Suffix := gsub("\\(.*?\\)", "", Suffix)]

  # Select and return the desired columns
  result <- dt[, .(PlantletID, Prefix, Suffix, EmbryoNo)]

  return(result)
}



#' @export
tc_plantlet_data <- function(dt, embryo_col, number_per_tube, number_of_copies) {
  # Ensure dt is a data.table
  if (!"data.table" %in% class(dt)) {
    setDT(dt)
  }

  # Handle different cases for number_per_tube
  if (number_per_tube == "single plant" && number_of_copies > 0) {
    # Replicate rows based on the number of copies
    dt <- dt[rep(seq_len(nrow(dt)), each = number_of_copies)]
  } else if (number_per_tube == "3 plants/test tube") {
    # Calculate the number of rows to replicate for 3 plants per tube
    dt[, n := ceiling(get(embryo_col) / 3)]
    dt <- dt[rep(seq_len(.N), n)][, n := NULL]  # Replicate rows and drop the helper column
  } else if (number_per_tube == "6 plants/test tube") {
    # Calculate the number of rows to replicate for 6 plants per tube
    dt[, n := ceiling(get(embryo_col) / 6)]
    dt <- dt[rep(seq_len(.N), n)][, n := NULL]  # Replicate rows and drop the helper column
  } else if (number_per_tube == "Equal to # of embryo_col") {
    # Replicate rows based on the exact value of embryo_col
    dt <- dt[rep(seq_len(nrow(dt)), get(embryo_col))]
  }

  # Split PlantletID into Prefix, Suffix, and EmbryoNo
  split_plantlet <- tstrsplit(as.character(dt$PlantletID), "_", fixed = TRUE)
  dt[, c("Prefix", "Suffix", "EmbryoNo") := .(split_plantlet[[1]], split_plantlet[[2]], split_plantlet[[3]])]

  # Clean up Suffix by removing parentheses and their contents
  dt[, Suffix := gsub("\\((.*?)\\)", "\\1", Suffix)]

  # Select the desired columns
  result <- dt[, .(PlantletID, Prefix, Suffix, EmbryoNo)]

  return(result)
}



# Summary Table
#' @export
summary_table <- function(data_file, start_date, end_date) {
  file_path <- paste0("./app/data/", data_file, ".rds")

  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }

  # Load data as a data.table
  data <- readRDS(file_path)

  # Filter out rows with missing key columns
  data[,c(grep("Plot", names(data), value = T), "Cycle")] <- NULL
  setDT(data)

  result <- data[
    Female_Genotype != "" & Male_Genotype != "" & Location != "",
  ]

  # Apply date range filtering if dates are valid
  if (!is.null(start_date) && all(!is.na(start_date))) {
    result <- result[
      First_Pollination_Date >= start_date & First_Pollination_Date <= end_date
    ]
  }

  # Add derived columns
  result[, `:=`(
    Banana_Bunches = as.integer(!is.na(Bunch_Harvest_Date)),
    Year_of_Pollination = as.integer(lubridate::year(First_Pollination_Date)),
    Month_of_Pollination = lubridate::month(First_Pollination_Date)
  )]

  # Remove unnecessary columns
  result[, c("Bunch_Harvest_Date", "Seed_Extraction_Date",
             "Embryo_Rescue_Date", "Germination_Date") := NULL]

  # Create month mapping and join with result
  month_data <- data.table(
    Month_of_Pollination = 1:12,
    month = factor(month.name, levels = month.name)
  )
  result <- merge(result, month_data, by = "Month_of_Pollination", all.x = TRUE)
  result[, Month_of_Pollination := month]
  result[, month := NULL]

  # Select and order relevant columns
  selected_cols <- c(
    "Location", "Crossnumber", "Female_Genotype", "Female_Ploidy", "Female_Sub_Group",
    "Male_Genotype", "Male_Ploidy", "Male_Sub_Group", "Cross_Type", "First_Pollination_Date",
    "Year_of_Pollination", "Month_of_Pollination", "Number_of_Repeats", "Banana_Bunches",
    "Total_Seeds", "Good_Seeds", "Number_of_Embryo_Rescued", "Number_of_Embryo_Germinating",
    "Number_of_Subcultures", "Number_Rooting", "Number_Sent_Out", "Weaning_2_Plantlets",
    "Number_in_Screenhouse", "Number_in_hardening", "Number_in_Openfield"
  )
  result <- result[, ..selected_cols]

  # Set data types
  factor_cols <- c("Location", "Cross_Type", "Female_Genotype", "Female_Sub_Group",
                   "Female_Ploidy", "Male_Genotype", "Male_Sub_Group", "Male_Ploidy",
                   "Year_of_Pollination", "Month_of_Pollination")
  num_cols <- grep("Number|Total_Seeds|Good_Seeds|Weaning_2_Plantlets", names(result), value = TRUE)

  result[, (factor_cols) := lapply(.SD, as.factor), .SDcols = factor_cols]
  result[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]

  return(result)
}



# Activity specific summaries
#' @export
summarize_activity <- function(data, group_by_col, activity) {
  # Ensure data is a data.table
  if (!"data.table" %in% class(data)) {
    data <- data.table::setDT(data)
  }

  # Group data by specified columns
  result <- data[, {
    # Calculate summary based on the activity
    result_col <- switch(
      activity,
      "first_pollination" = .N,
      "banana_bunches" = .N,
      "seed_extraction" = sum(Total_Seeds, na.rm = TRUE),
      "good_seeds" = sum(Good_Seeds, na.rm = TRUE),
      "embryo_rescue" = sum(Number_of_Embryo_Rescued, na.rm = TRUE),
      "embryo_germination" = sum(Number_of_Embryo_Germinating, na.rm = TRUE),
      "subcultures" = sum(embryo_col, na.rm = TRUE),
      "rooting" = sum(Number_Rooting, na.rm = TRUE),
      "weaning1" = sum(Number_Sent_Out, na.rm = TRUE),
      "weaning2" = sum(Number_in_Weaning_2, na.rm = TRUE),
      "screenhouse" = sum(Number_in_Screenhouse, na.rm = TRUE),
      "hardening" = sum(Number_in_Hardening, na.rm = TRUE),
      "openfield" = sum(Number_in_Openfield, na.rm = TRUE),
      NA_real_
    )
    list(result_col)
  }, by = group_by_col]

  # Dynamically rename the result column
  setnames(
    result,
    "result_col",
    paste0("Number_of_", gsub(" ", "_", activity))
  )

  return(result)
}


