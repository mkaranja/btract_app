
box::use(
  shiny[reactive],
  dplyr,
  data.table[setDT],
)

#'
#' #' @export
#' filter_data <- function(dataset, start, end, site) {
#'   dt <- readRDS(paste0("./app/data/", dataset, ".rds")) |>
#'     dplyr::filter(Location == site)
#'
#'   # Find the date column
#'   date_col <- grep("Date", names(dt), value = TRUE)[1]
#'
#'   # Filter by date range
#'   dt <- dt |>
#'     dplyr::filter(!!rlang::sym(date_col) >= start & !!rlang::sym(date_col) <= end)
#'
#'   return(dt)
#' }
#'
#'
#' @export
load_data <- function(data_file, cols = NULL) {
  file_path <- paste0("./app/data/", data_file, ".rds")

  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }

  data <- readRDS(file_path) |> unique()

  if (!is.null(cols)) {
    data <- data[, cols]
  }

  return(data)
}


#'
#'
#' @export
highchart_data <- function(data, stage, cols, date_col, num_col = NULL, start_date, end_date, site = NULL){
  data <- data |> # dplyr::select(data, all_of(cols)) |>
    dplyr::rename("Date" = date_col)

  if(length(site) > 0){
    data <- data |>
      dplyr::filter(Location %in% site)
  }

  data <- data |>
    dplyr::filter(dplyr::between(Date, start_date, end_date)) |>
    dplyr::mutate(
      Yearly = lubridate::year(Date),
      Monthly = lubridate::month(Date),
      Daily = lubridate::day(Date)
    )

  if(stage %in%  c("Crosses", "Banana Bunches") ){
    data$Number <- rep(1, times=nrow(data))
  } else {
    data <- data  |>
      dplyr::filter(!is.na(num_col))
    data$Number <- data[[num_col]]
  }

  return(data)
}

#' @export
grouped_data <- function(data, group){
  if(group == "daily"){
    data <- data |>
      dplyr::group_by(Location, Date) |>
      dplyr::summarize(number = sum(Number, na.rm = T), .groups = 'drop')
    data$Time <- data$Date
    group_name <- "Daily"
  } else if(group == "monthly"){
    data <- within(data, Time <- sprintf("%d-%02d", Yearly, Monthly))
    data <- data |>
      dplyr::group_by(Location,Time) |>
      dplyr::summarize(number = sum(Number, na.rm = T), .groups = 'drop')
    group_name = "Monthly"
  }  else if(group == "yearly"){
    data <- data |>
      dplyr::group_by(Location, Yearly) |>
      dplyr::summarize(number = sum(Number, na.rm = T), .groups = 'drop')
    data$Time = data$Yearly
    group_name = "Yearly"
  }
  return(data)
}

# TC labels for download
#' @export
# TC labels for download
#' @export
tc_embryo_data <- function(dt, number_per_tube, number_of_copies, rows_selected) {

  # Filter selected rows efficiently using data.table
  if (!is.null(rows_selected)) {
    dt <- dt[rows_selected,]
  }

  dt <- data.table::setDT(dt)

  # Handle single plant labels (with replication if needed)
  # Define a function to handle row replication based on number of embryos rescued
  if (number_per_tube == 'single plant') {
    # For single plant, replicate rows based on the number of copies
    if (number_of_copies > 0) {
      dt <- dt[rep(1:nrow(dt), each = number_of_copies)]
      data.table::setorder(dt, 1)  # Sort rows if needed
    }
  } else {
    # For other cases, handle by dividing Number_of_Embryo_Rescued by the plants per tube
    plants_per_tube <- switch(number_per_tube,
                              "3 plants/test tube" = 3,
                              "6 plants/test tube" = 6,
                              "Equal to # Embryo Rescued" = 1)  # Default to 1 for "Equal to # Embryo Rescued"

    if (plants_per_tube > 1) {
      dt[, n := pmin(ceiling(get("Number_of_Embryo_Rescued") / plants_per_tube), get("Number_of_Embryo_Rescued"))]
      dt <- dt[rep(1:nrow(dt), times = dt$n)]  # Replicate rows efficiently
      dt[, n := NULL]  # Remove temporary column 'n'
    } else if (plants_per_tube == 1) {
      # Replicate rows based on Number_of_Embryo_Rescued for "Equal to # Embryo Rescued"
      dt <- dt[rep(1:nrow(dt), times = get("Number_of_Embryo_Rescued"))]
    }
  }



  # Split Crossnumber and clean up using data.table's vectorized operations
  split_crossnumber <- strsplit(as.character(dt$Crossnumber), "_")
  crossnumber_df <- data.table::rbindlist(lapply(split_crossnumber, function(x) data.table(Prefix = x[1], Suffix = gsub("[()]", "", x[2]))))

  # Combine the result and return it
  data.table::setDT(dt)
  data.table::setDT(crossnumber_df)
  result <- cbind(dt, crossnumber_df)[, .(Crossnumber, Prefix, Suffix)]

  return(result)
}

#' @export
tc_embryo_data2 <- function(dt, number_per_tube, number_of_copies, rows_selected=NULL) {

  # Filter selected rows efficiently using data.table
  if (!is.null(rows_selected)) {
    dt <- dt[rows_selected,]
  }

  dt <- data.table::setDT(dt)

  # single plants labels
  if(number_per_tube == 1){
    dt <- dt
  } else if(number_per_tube == 2){
    # Replicate rows based on Number_of_Embryo_Rescued for "Equal to # Embryo Rescued"
    dt <- dt[rep(1:nrow(dt), times = get("Number_of_Embryo_Rescued"))]
  } else {
    dt[, n := ceiling(Number_of_Embryo_Rescued / as.integer(number_per_tube))]
    dt <- dt[rep(1:.N, times = n)]  # Efficiently replicate rows
    dt[, n := NULL]  # Remove temporary column

    # dt <- dt[, n := pmin(ceiling(get("Number_of_Embryo_Rescued") / integer(number_per_tube)), get("Number_of_Embryo_Rescued"))]
    # dt <- dt[rep(1:nrow(dt), times = dt$n)]  # Replicate rows efficiently
    # dt[, n := NULL]  # Remove temporary column 'n'
  }

  # Handle row replication if necessary
  if (number_of_copies > 1) {
    # Efficiently replicate rows using data.table's `rep` function
    dt <- dt[rep(1:nrow(dt), each = number_of_copies)]
    # data.table::setorder(dt, 1)  # Optionally order the rows
  }

  # download formats
  df <- data.frame(stringr::str_split_fixed(dt$Crossnumber,"_",2))
  colnames(df) <- c('Prefix','Suffix')
  df$Suffix <- gsub("[)]","",(gsub("[(]","", df$Suffix)))

  result <- cbind(dt,df) |>
    dplyr::select(Crossnumber, Prefix, Suffix)

  return(result)

}

#' @export
tc_embryo_germination <- function(dt, number_of_copies, rows_selected=NULL) {

  dt <- data.table::setDT(dt)

  # Filter selected rows efficiently using data.table
  if (!is.null(rows_selected)) {
    dt <- dt[rows_selected,]
  }

  # Handle row replication if necessary
  if (number_of_copies > 1) {
    # Efficiently replicate rows using data.table's `rep` function
    dt <- dt[rep(1:nrow(dt), each = number_of_copies)]
    # data.table::setorder(dt, 1)  # Optionally order the rows
  }

  # Split PlantletID into Prefix, Suffix, and EmbryoNo using data.table's vectorized operations
  split_plantlet <- strsplit(as.character(dt$PlantletID), "_")
  plantlet_df <- data.table::rbindlist(lapply(split_plantlet, function(x) data.table::data.table(Prefix = x[1], Suffix = gsub("[()]", "", x[2]), EmbryoNo = x[3])))

  # Combine the result with the split data and select relevant columns
  result <- cbind(dt, plantlet_df)|>
    dplyr::select(PlantletID, Prefix, Suffix, EmbryoNo)
  #[, .(PlantletID, Prefix, Suffix, )]

  return(result)
}

#' @export
tc_plantlet_data <- function(dt, embryo_col, number_per_tube, number_of_copies, rows_selected = NULL) {

  dt <- data.table::setDT(dt)

  # Filter selected rows efficiently using data.table
  if (!is.null(rows_selected)) {
    dt <- dt[rows_selected,]
  }

  # single plants labels
  if(number_per_tube == 1){
    dt <- dt
  } else if(number_per_tube == 2){
    dt <- dt[rep(1:nrow(dt), dt[[embryo_col]]), ]  # Replicate rows based on 'embryo_col'
  } else {
    dt[, n := ceiling(get(embryo_col) / as.integer(number_per_tube))]  # Calculate how many rows to replicate
    dt <- tidyr::uncount(dt, weights = n)  # Replicate rows based on 'n'

    # dt[, n := pmin(ceiling(dt[[embryo_col]] / integer(number_per_tube)), dt[[embryo_col]])]  # Efficient vectorized calculation
    # dt <- tidyr::uncount(dt, weights = n)  # Replicate rows based on 'n'
  }

  # Handle row replication if necessary
  if (number_of_copies > 1) {
    dt <- dt[rep(1:nrow(dt), each = number_of_copies)]
  }

  # Split PlantletID into Prefix, Suffix, and EmbryoNo (vectorized)
  df <- stringr::str_split_fixed(dt$PlantletID, "_", 3)
  colnames(df) <- c('Prefix', 'Suffix', 'EmbryoNo')
  df <- data.frame(df)
  df$Suffix <- gsub("\\((.*?)\\)", "\\1", df$Suffix)  # Clean up parentheses

  # Combine the original dt with the new columns and select the relevant columns
  result <- cbind(dt, df)[, .(PlantletID, Prefix, Suffix, EmbryoNo, Ploidy_Level)]

  return(result)
}


#' @export
tc_data <- function(dt, number_per_tube, number_of_copies = 1,
                    rows_selected = NULL, embryo_col = NULL,
                    data_type = c("embryo", "plantlet")) {

  # Validate inputs
  data_type <- match.arg(data_type)
  if (data_type == "plantlet" && is.null(embryo_col)) {
    stop("embryo_col must be specified for plantlet data processing")
  }

  # Convert to data.table if not already
  dt <- data.table::setDT(dt)

  # Filter selected rows if specified
  if (!is.null(rows_selected)) {
    dt <- dt[rows_selected]
  }

  # Handle row replication based on configuration
  if (number_per_tube %in% c('single plant', '1 plant/test tube')) {
    if (number_of_copies > 0) {
      dt <- dt[rep(1:.N, each = number_of_copies)]
    }
  } else {
    plants_per_tube <- switch(number_per_tube,
                              "3 plants/test tube" = 3,
                              "6 plants/test tube" = 6,
                              1  # Default for "Equal to # Embryo Rescued" or "Equal # of plantlets"
    )

    count_col <- if (data_type == "plantlet") embryo_col else "Number_of_Embryo_Rescued"

    if (plants_per_tube > 1) {
      dt[, n := pmin(ceiling(get(count_col) / plants_per_tube), get(count_col))]
      dt <- dt[rep(1:.N, times = n)]
      dt[, n := NULL]
    } else {
      dt <- dt[rep(1:.N, times = get(count_col))]
    }
  }

  # Process labels based on data type
  if (data_type == "embryo") {
    # Split Crossnumber into Prefix and Suffix
    split_col <- "Crossnumber"
    result_cols <- c("Prefix", "Suffix")
    split_pattern <- "_"
  } else {
    # Split PlantletID into Prefix, Suffix and EmbryoNo
    split_col <- "PlantletID"
    result_cols <- c("Prefix", "Suffix", "EmbryoNo")
    split_pattern <- "_"
  }

  # Efficient splitting using data.table
  split_result <- data.table::tstrsplit(dt[[split_col]], split_pattern, fixed = TRUE)
  #names(split_result) <- result_cols

  # Clean up parentheses in Suffix
  split_result[["Suffix"]] <- gsub("[()]", "", split_result[["Suffix"]])

  # Combine results
  result <- cbind(dt, split_result)

  # Select relevant columns
  if (data_type == "embryo") {
    return(result[, .("Crossnumber", "Prefix", "Suffix")])
  } else {
    return(result[, .("PlantletID", "Prefix", "Suffix", "EmbryoNo")])
  }
}

# Summary Table
#' @export
summary_table <- function(data_file) {
  file_path <- paste0("./app/data/", data_file, ".rds")

  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }

  data <- readRDS(file_path) |> unique()

  # Ensure data is properly filtered
  result <- data |>
    dplyr::select(-contains("Plot"), -Cycle) |>
    dplyr::filter(Female_Genotype != '', Male_Genotype != '', Location != '')

  # Add derived columns and remove unnecessary columns
  result <- result |>
    dplyr::mutate(
      Banana_Bunches = ifelse(!is.na(Bunch_Harvest_Date), 1, 0),
      Year_of_Pollination = as.integer(lubridate::year(First_Pollination_Date)),
      Month_of_Pollination = lubridate::month(First_Pollination_Date)
    ) |>
    dplyr::select(
      -Bunch_Harvest_Date, -Seed_Extraction_Date,
      -Embryo_Rescue_Date, -Germination_Date
    )

  # Add and align month data
  month_data <- data.frame(
    month = 1:12, x = LETTERS[1:12], Month_of_Pollination = 1:12
  ) |>
    dplyr::mutate(month = factor(month.name[month], levels = month.name)) |>
    dplyr::arrange(month)

  result <- month_data |>
    dplyr::left_join(result, by = "Month_of_Pollination") |>
    dplyr::select(-c(x, Month_of_Pollination)) |>
    dplyr::rename(Month_of_Pollination = month)

  # Select relevant columns
  result <- result |>
    dplyr::select(
      Location, Crossnumber, Female_Genotype, Female_Ploidy, Female_Sub_Group, Male_Genotype,
      Male_Ploidy, Male_Sub_Group, Cross_Type, First_Pollination_Date, Year_of_Pollination,
      Month_of_Pollination, Number_of_Repeats, Banana_Bunches, Total_Seeds, Good_Seeds,
      Number_of_Embryo_Rescued, Number_of_Embryo_Germinating, Number_of_Subcultures,
      Number_Rooting, Number_Sent_Out, Weaning_2_Plantlets, Number_in_Screenhouse,
      Number_in_hardening, Number_in_Openfield
    )
  # set data types
  factor_cols <- c("Location", "Cross_Type", "Female_Genotype", "Female_Sub_Group", "Female_Ploidy",
                   "Male_Genotype", "Male_Sub_Group", "Male_Ploidy", "Year_of_Pollination", "Month_of_Pollination")
  num_cols <- c(grep("Number", names(data), value = T), "Total_Seeds", "Good_Seeds", "Weaning_2_Plantlets" )

  result <- result |>
    dplyr::mutate_at(dplyr::vars(dplyr::all_of(factor_cols)), as.factor) |>
    dplyr::mutate_at(dplyr::vars(dplyr::all_of(num_cols)), as.numeric)

  return(result)
}


# Activity specific summaries
#' @export
summarize_activity <- function(data, group_by_col, activity) {
  result <- data |>
    dplyr::group_by(across(all_of(group_by_col)))

  # Define the summary logic for each activity
  result <- result |>
    summarise(
      result_col = case_when(
        activity == "first_pollination" ~ n(),
        activity == "banana_bunches" ~ n(),
        activity == "seed_extraction" ~ sum(Total_Seeds, na.rm = TRUE),
        activity == "seed_extraction" ~ sum(Good_Seeds, na.rm = TRUE),
        activity == "embryo_rescue" ~ sum(Number_of_Embryo_Rescued, na.rm = TRUE),
        activity == "embryo_germination" ~ sum(Number_of_Embryo_Germinating, na.rm = TRUE),
        activity == "subcultures" ~ sum(embryo_col, na.rm = TRUE),
        activity == "rooting" ~ sum(Number_Rooting, na.rm = TRUE),
        activity == "weaning1" ~ sum(Number_Sent_Out, na.rm = TRUE),
        activity == "weaning2" ~ sum(Number_in_Weaning_2, na.rm = TRUE),
        activity == "screenhouse" ~ sum(Number_in_Screenhouse, na.rm = TRUE),
        activity == "hardening" ~ sum(Number_in_Hardening, na.rm = TRUE),
        activity == "openfield" ~ sum(Number_in_Openfield, na.rm = TRUE),
        TRUE ~ NA_real_
      )
    ) |>
    rename(!!paste("Number_of", gsub(" ", "_", activity)) := result_col)  # Dynamically rename the column based on activity

  return(result)
}

