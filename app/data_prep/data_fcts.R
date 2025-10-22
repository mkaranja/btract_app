box::use(
  data.table,
  dplyr,
  stringr[str_extract],
  )

# Define genotype mappings in a list for better maintainability
genotype_mappings <- list(
  "ITC0712-Aacv_Rose" = c("AAcv Rose", "AAcv-Rose", "CV Rose", "C_V_rose", "Cultivar-Rose",
                          "cv-Rose", "cv.Rose", "cvrose", "cv-Rose", "ITC0712",
                          "ITC0712-Cv-Rose", "ITC0712 Cv Rose", "CV.Rose"),
  "ITC0609-Pahang" = c("ITC0609", "Pahang", "Pisang Pahang", "ITC0609 Pahang"),
  "ITC0249-Calcutta 4" = c("Calcutta 4", "ITC0249", "ITC0249 Calcutta 4"),
  "ITC0253-Borneo" = c("Borneo", "ITC0253", "ITC0253 Borneo"),
  "ITC0766-Paliama" = c("ITC0766", "Paliama"),
  "ITC1460-Ijihu_Inkundu" = c("ITC1460", "Ijihu nkundu", "ITC.1460_Ijihu Inkundu",
                              "ITC1460-Ijihu Inkundu", "Ijihu Inkundu", "Ijihu-Inkundu",
                              "ITC1460-Ijihu_nkundu"),
  "ITC1468-kahuti" = c("ITC1468", "Kahuti"),
  "ITC1355-Kazirakwe" = c("ITC1355"),
  "ITC1354-Enzirabahima" = c("ITC1354"),
  "TMB2X9128-3" = c("TMB2x 9128-3"),
  "ITC0090-Tjau_Lagada" = c("Tjau Lagada"),
  "ITC0281-Akondro_Mainty" = c("Akondro mainty"),
  "ITC1559-Huti_green_bell" = c("Huti-Green"),
  "ITC0966-Zebrina_GF" = c("Zebrina GF", "ITC0966 Zebrina (GF)", "ITC0966-Zebrina_(G.F.)",
                           "ZEBRINA G.F,ZEBRINA GF", "ZEBRINA-GF", "Zebrina (G.F.)",
                           "Zebrina G.F", "Zebrina-G-F", "Zebrina-GF", "ZebrinaG.F"),
  "02145/1320" = c("IITA 02145/1320"),
  "Huti-white" = c("Huti-White", "Huti - White"),
  "ITC0299-guyod" = c("Guyod"),
  "ITC0393-Truncata" = c("Truncata"),
  "ITC0943-Kwaro" = c("Kwaro"),
  "ITC1466-Nshonowa" = c("Nshonowa"),
  "ITC0774-Yenai" = c("Yenai"),
  "ITC0600-Waimara" = c("Waimara"),
  "ITC1252-Datil" = c("Datil"),
  "ITC1564-Mlali" = c("Mlali"),
  "ITC0085-Nakitengwa" = c("Nakitengwa"),
  "Muraru (Mlalu)" = c("Muraru Mlalu"),
  "KABUCURAGYE" = c("Kabucuragye"),
  "ILALYI" = c("Ilalyi"),
  "ITC0425-SH-3142" = c("ITC0425 SH-3142", "ITC0425-SH-3142", "SH 3142", "SH-3142", "SH3142"),
  "ITC1448-IITA_hybrid_2145/1320" = c("ITC1448 IITA hybrid 2145/1320", "IITA hybrid 2145/1320",
                                      "IITA-hybrid-2145-1320"),
  "ITC1121-Pisang_Lilin" = c("ITC1121 Pisang Lilin", "ITC1121-Pisang_Lilin", "P_lilin",
                             "Pisang Lilin", "Pisang lilin", "Pisang-Lilin", "Pisang-lilin",
                             "Pisang_lilin"),
  "ITC1455-Mchare Mlelembo" = c("ITC.1455_Mshale Mlelembo", "Mchale Mlelembo",
                                "Mchare Mlelembo", "Mshale Mlelembo", "Mshale-Mlelembo",
                                "Mshare Mlelembo")
)

# Function to standardize genotypes
#' @export
standardize_genotypes <- function(column) {
  for (standard_name in names(genotype_mappings)) {
    column <- ifelse(column %in% genotype_mappings[[standard_name]],
                     standard_name,
                     column)
  }
  return(column)
}

#' @export
clean_genotype_columns <- function(df) {
  df |>
    dplyr$mutate(dplyr$across(c(Male_Genotype, Female_Genotype),
                  ~ dplyr$case_when(
                    # Case 1: Standard HTML link pattern
                    grepl('^<a href=".*?".*?>.*?</a>$', .) ~
                      str_extract(., "(?<=>)[^<]+"),

                    # Case 2: Already clean text
                    TRUE ~ .
                  ))
    )
}


read_data <- function(data_dir,
                              dataset_type,
                              col_specs,
                              file_pattern = NULL,
                              ...) {
  # Input validation
  if (!dir.exists(data_dir)) {
    stop("Directory does not exist: ", data_dir)
  }

  if (missing(dataset_type) || !is.character(dataset_type)) {
    stop("dataset_type must be a character string")
  }

  if (missing(col_specs) || !is.list(col_specs)) {
    stop("col_specs must be a named list of column specifications")
  }

  # Set default file pattern if not provided
  if (is.null(file_pattern)) {
    file_pattern <- sprintf("*%s*.csv$", dataset_type)
  }

  # Find matching files
  file_paths <- list.files(
    path = data_dir,
    pattern = file_pattern,
    full.names = TRUE,
    ignore.case = TRUE
  )

  print(file_paths)
  if (length(file_paths) == 0) {
    warning("No files found matching pattern: ", file_pattern)
    return(data.table())
  }

  # Process each file
  result_list <- lapply(file_paths, function(fp) {
    message("Processing: ", basename(fp))

    # Read data
    dt <- data.table::fread(fp, ...)

    # Apply column specifications
    for (col_name in names(col_specs)) {
      spec <- col_specs[[col_name]]

      # Handle column renaming
      target_name <- if (!is.null(spec$new_name)) spec$new_name else col_name

      if (col_name %in% names(dt)) {
        # Rename if needed
        if (!is.null(spec$new_name)) {
          data.table::setnames(dt, col_name, target_name)
        }

        # Apply type conversion
        switch(
          spec$type,
          "character" = dt[, (target_name) := as.character(get(target_name))],
          "numeric" = dt[, (target_name) := as.numeric(get(target_name))],
          "integer" = dt[, (target_name) := as.integer(get(target_name))],
          "Date" = {
            origin <- if (!is.null(spec$origin)) spec$origin else "1970-01-01"
            dt[, (target_name) := as.Date(get(target_name), origin = origin)]
          },
          "factor" = {
            levels <- if (!is.null(spec$levels)) spec$levels else sort(unique(get(target_name)))
            dt[, (target_name) := factor(get(target_name), levels = levels)]
          }
        )
      }
    }

    # Add metadata
    # dt[, `:=`(
    #   source_file = basename(fp),
    #   processing_date = Sys.Date(),
    #   dataset_type = dataset_type
    # )]
    print(dim(dt))

    return(dt)
  })

  # Combine all files
  combined_data <- plyr::rbind.fill(result_list)

  if (nrow(combined_data) == 0) {
    warning("No data found after processing files")
  } else {
    message(sprintf(
      "Successfully processed %d files with %d rows for %s data",
      length(file_paths),
      nrow(combined_data),
      dataset_type
    ))
  }

  return(combined_data)
}



read_datasets <- function(data_dir,
                              dataset_type,
                              col_specs,
                              file_pattern = NULL,
                              ordered_factors = FALSE,
                              ...) {
  # Input validation
  if (!dir.exists(data_dir)) {
    stop("Directory does not exist: ", data_dir)
  }

  if (missing(dataset_type) || !is.character(dataset_type)) {
    stop("dataset_type must be a character string")
  }

  if (missing(col_specs) || !is.list(col_specs)) {
    stop("col_specs must be a named list of column specifications")
  }

  # Set default file pattern if not provided
  if (is.null(file_pattern)) {
    file_pattern <- sprintf("*%s*.csv$", dataset_type)
  }

  # Find matching files
  file_paths <- list.files(
    path = data_dir,
    pattern = file_pattern,
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(file_paths) == 0) {
    warning("No files found matching pattern: ", file_pattern)
    return(data.table())
  }

  # Process each file
  result_list <- lapply(file_paths, function(fp) {
    message("Processing: ", basename(fp))

    # Read data
    dt <- data.table::fread(fp, ...)

    # Apply column specifications
    for (col_name in names(col_specs)) {
      spec <- col_specs[[col_name]]
      target_name <- if (!is.null(spec$new_name)) spec$new_name else col_name

      if (col_name %in% names(dt)) {
        # Rename if needed
        if (!is.null(spec$new_name)) {
          data.table::setnames(dt, col_name, target_name)
        }

        # Apply type conversion
        switch(
          spec$type,
          "character" = dt[, (target_name) := as.character(get(target_name))],
          "numeric" = dt[, (target_name) := as.numeric(get(target_name))],
          "integer" = dt[, (target_name) := as.integer(get(target_name))],
          "Date" = {
            origin <- if (!is.null(spec$origin)) spec$origin else "1970-01-01"
            dt[, (target_name) := as.Date(get(target_name), origin = origin)]
          },
          "factor" = {
            # Handle factor creation with optional levels and ordering
            col_data <- dt[[target_name]]

            # Get levels if specified, otherwise use sorted unique values
            levels <- if (!is.null(spec$levels)) {
              spec$levels
            } else {
              if (is.factor(col_data)) {
                levels(col_data)
              } else {
                sort(unique(col_data))
              }
            }

            # Determine if ordered
            ordered <- if (!is.null(spec$ordered)) spec$ordered else ordered_factors

            # Create factor
            dt[, (target_name) := factor(
              get(target_name),
              levels = levels,
              ordered = ordered
            )]
          },
          warning("Unknown type specification: ", spec$type)
        )
      }
    }

    # Add metadata
    # dt[, `:=`(
    #   source_file = basename(fp),
    #   processing_date = Sys.Date(),
    #   dataset_type = dataset_type
    # )]

    return(dt)
  })

  # Combine all files
  combined_data <- plyr::rbind.fill(result_list, fill = TRUE)

  if (nrow(combined_data) == 0) {
    warning("No data found after processing files")
  } else {
    message(sprintf(
      "Successfully processed %d files with %d rows for %s data",
      length(file_paths),
      nrow(combined_data),
      dataset_type
    ))

    # Report on factor columns
    factor_cols <- names(which(sapply(combined_data, is.factor)))
    if (length(factor_cols) > 0) {
      message("\nFactor columns created:")
      for (col in factor_cols) {
        spec <- col_specs[[col]]
        if (is.null(spec)) {
          # Handle renamed columns
          orig_col <- names(which(sapply(col_specs, function(x) x$new_name == col)))
          spec <- if (length(orig_col)) col_specs[[orig_col]] else NULL
        }

        if (!is.null(spec) && spec$type == "factor") {
          ordered_status <- ifelse(is.ordered(combined_data[[col]]),
                                   "ordered", "unordered")
          message(sprintf(
            "- %s (%s, %d levels: %s)",
            col,
            ordered_status,
            nlevels(combined_data[[col]]),
            paste(levels(combined_data[[col]]), collapse = ", ")
          ))
        }
      }
    }
  }

  return(combined_data)
}


read_dataset <- function(data_dir,
                              dataset_type,
                              col_specs,
                              file_pattern = NULL,
                              ordered_factors = FALSE,
                              id_var = "PlantletID",
                              verbose = TRUE,
                              ...) {
  # Input validation
  if (!dir.exists(data_dir)) {
    stop("Directory does not exist: ", data_dir)
  }

  if (missing(dataset_type) || !is.character(dataset_type)) {
    stop("dataset_type must be a character string")
  }

  if (missing(col_specs) || !is.list(col_specs)) {
    stop("col_specs must be a named list of column specifications")
  }

  # Set default file pattern if not provided
  if (is.null(file_pattern)) {
    file_pattern <- sprintf("*%s*.csv$", dataset_type)
  }

  # Find matching files
  file_paths <- list.files(
    path = data_dir,
    pattern = file_pattern,
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(file_paths) == 0) {
    warning("No files found matching pattern: ", file_pattern)
    return(data.table())
  }

  # Process each file
  result_list <- lapply(file_paths, function(fp) {
    if (verbose) message("Processing: ", basename(fp))

    # Read data
    dt <- data.table::fread(fp, ...)

    # Apply column specifications
    for (col_name in names(col_specs)) {
      spec <- col_specs[[col_name]]
      target_name <- if (!is.null(spec$new_name)) spec$new_name else col_name

      if (col_name %in% names(dt)) {
        # Rename if needed
        if (!is.null(spec$new_name)) {
          data.table::setnames(dt, col_name, target_name)
        }

        # Apply type conversion
        switch(
          spec$type,
          "character" = dt[, (target_name) := as.character(get(target_name))],
          "numeric" = dt[, (target_name) := as.numeric(get(target_name))],
          "integer" = dt[, (target_name) := as.integer(get(target_name))],
          "Date" = {
            origin <- if (!is.null(spec$origin)) spec$origin else "1970-01-01"
            dt[, (target_name) := as.Date(get(target_name), origin = origin)]
          },
          "factor" = {
            # Handle factor creation with optional levels and ordering
            col_data <- dt[[target_name]]

            # Get levels if specified, otherwise use sorted unique values
            levels <- if (!is.null(spec$levels)) {
              spec$levels
            } else {
              if (is.factor(col_data)) {
                levels(col_data)
              } else {
                sort(unique(col_data[!is.na(col_data)]))
              }
            }

            # Determine if ordered
            ordered <- if (!is.null(spec$ordered)) spec$ordered else ordered_factors

            # Create factor
            dt[, (target_name) := factor(
              get(target_name),
              levels = levels,
              ordered = ordered
            )]
          },
          if (verbose) warning("Unknown type specification: ", spec$type)
        )
      }
    }

    # Add metadata
    dt[, `:=`(
      source_file = basename(fp),
      processing_date = Sys.Date(),
      dataset_type = dataset_type
    )]

    return(dt)
  })

  # Combine all files
  combined_data <- plyr::rbind.fill(result_list, fill = TRUE)

  if (nrow(combined_data) == 0) {
    warning("No data found after processing files")
    return(combined_data)
  }

  # Check for ID variable presence
  if (!id_var %in% names(combined_data)) {
    warning("ID variable '", id_var, "' not found in dataset")
    return(combined_data)
  }

  # Handle duplicates based on ID variable
  if (anyDuplicated(combined_data[[id_var]])) {
    if (verbose) message("\nProcessing duplicates based on ", id_var)

    # Calculate number of missing values per row
    combined_data[, missing_count := rowSums(is.na(.SD)), .SDcols = setdiff(names(combined_data), c(id_var, "source_file", "processing_date", "dataset_type"))]

    # Order by ID and missing count (fewest missing first)
    data.table::setorderv(combined_data, c(id_var, "missing_count"))

    # Keep first occurrence (with fewest missing values)
    combined_data <- combined_data[!duplicated(combined_data[[id_var]]), ]

    # Remove temporary column
    combined_data[, missing_count := NULL]

    if (verbose) {
      dup_count <- sum(duplicated(combined_data[[id_var]]))
      message("Removed duplicate ", id_var, "s, keeping rows with fewest missing values")
    }
  }

  if (verbose) {
    message(sprintf(
      "\nFinal dataset: %d rows with %d unique %s values",
      nrow(combined_data),
      uniqueN(combined_data[[id_var]]),
      id_var
    ))

    # Report on factor columns
    factor_cols <- names(which(sapply(combined_data, is.factor)))
    if (length(factor_cols) > 0) {
      message("\nFactor columns created:")
      for (col in factor_cols) {
        spec <- col_specs[[col]]
        if (is.null(spec)) {
          # Handle renamed columns
          orig_col <- names(which(sapply(col_specs, function(x) x$new_name == col)))
          spec <- if (length(orig_col)) col_specs[[orig_col]] else NULL
        }

        if (!is.null(spec) && spec$type == "factor") {
          ordered_status <- ifelse(is.ordered(combined_data[[col]]),
                                   "ordered", "unordered")
          message(sprintf(
            "- %s (%s, %d levels: %s)",
            col,
            ordered_status,
            nlevels(combined_data[[col]]),
            paste(levels(combined_data[[col]]), collapse = ", ")
          ))
        }
      }
    }
  }

  return(combined_data)
}


read_agri_dataset2 <- function(data_dir,
                              dataset_type,
                              col_specs,
                              file_pattern = NULL,
                              ordered_factors = FALSE,
                              id_var = "PlantletID",
                              verbose = TRUE,
                              ...) {
  # Input validation
  if (!dir.exists(data_dir)) {
    stop("Directory does not exist: ", data_dir)
  }

  if (missing(dataset_type) || !is.character(dataset_type)) {
    stop("dataset_type must be a character string")
  }

  if (missing(col_specs) || !is.list(col_specs)) {
    stop("col_specs must be a named list of column specifications")
  }

  # Set default file pattern if not provided
  if (is.null(file_pattern)) {
    file_pattern <- sprintf("*%s*.csv$", dataset_type)
  }

  # Find matching files
  file_paths <- list.files(
    path = data_dir,
    pattern = file_pattern,
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(file_paths) == 0) {
    warning("No files found matching pattern: ", file_pattern)
    return(data.table())
  }

  # Process each file
  result_list <- lapply(file_paths, function(fp) {
    if (verbose) message("Processing: ", basename(fp))

    # Read data
    dt <- data.table::fread(fp, ...)

    # Apply column specifications
    for (col_name in names(col_specs)) {
      spec <- col_specs[[col_name]]
      target_name <- if (!is.null(spec$new_name)) spec$new_name else col_name

      if (col_name %in% names(dt)) {
        # Rename if needed
        if (!is.null(spec$new_name)) {
          data.table::setnames(dt, col_name, target_name)
        }

        # Apply type conversion
        switch(
          spec$type,
          "character" = dt[, (target_name) := as.character(get(target_name))],
          "numeric" = dt[, (target_name) := as.numeric(get(target_name))],
          "integer" = dt[, (target_name) := as.integer(get(target_name))],
          "Date" = {
            origin <- if (!is.null(spec$origin)) spec$origin else "1970-01-01"
            dt[, (target_name) := as.Date(get(target_name), origin = origin)]
          },
          "factor" = {
            # Handle factor creation with optional levels and ordering
            col_data <- dt[[target_name]]

            # Get levels if specified, otherwise use sorted unique values
            levels <- if (!is.null(spec$levels)) {
              spec$levels
            } else {
              if (is.factor(col_data)) {
                levels(col_data)
              } else {
                sort(unique(col_data[!is.na(col_data)]))
              }
            }

            # Determine if ordered
            ordered <- if (!is.null(spec$ordered)) spec$ordered else ordered_factors

            # Create factor
            dt[, (target_name) := factor(
              get(target_name),
              levels = levels,
              ordered = ordered
            )]
          },
          if (verbose) warning("Unknown type specification: ", spec$type)
        )
      }
    }

    # Add metadata
    # dt[, `:=`(
    #   source_file = basename(fp),
    #   processing_date = Sys.Date(),
    #   dataset_type = dataset_type
    # )]

    return(dt)
  })

  # Combine all files
  combined_data <- plyr::rbind.fill(result_list)

  if (nrow(combined_data) == 0) {
    warning("No data found after processing files")
    return(combined_data)
  }

  # Check for ID variable presence
  if (!id_var %in% names(combined_data)) {
    warning("ID variable '", id_var, "' not found in dataset")
    return(combined_data)
  }

  # Handle duplicates based on ID variable
  if (anyDuplicated(combined_data[[id_var]])) {
    if (verbose) message("\nProcessing duplicates based on ", id_var)

    # Calculate number of missing values per row
    cols_to_check <- setdiff(names(combined_data),
                             c(id_var, "source_file", "processing_date", "dataset_type"))
    combined_data[, missing_count := rowSums(is.na(.SD)), .SDcols = cols_to_check]

    # Order by ID and missing count (fewest missing first)
    data.table::setorderv(combined_data, c(id_var, "missing_count"))

    # Keep first occurrence (with fewest missing values)
    combined_data <- combined_data[!duplicated(combined_data[[id_var]]), ]

    # Remove temporary column
    combined_data[, missing_count := NULL]

    if (verbose) {
      dup_count <- sum(duplicated(combined_data[[id_var]]))
      message("Removed ", dup_count, " duplicate ", id_var, "s, keeping rows with fewest missing values")
    }
  }

  if (verbose) {
    message(sprintf(
      "\nFinal dataset: %d rows with %d unique %s values",
      nrow(combined_data),
      data.table::uniqueN(combined_data[[id_var]]),
      id_var
    ))

    # Report on factor columns - fixed error in this section
    factor_cols <- names(which(sapply(combined_data, is.factor)))
    if (length(factor_cols) > 0) {
      message("\nFactor columns created:")
      for (col in factor_cols) {
        # Find the spec for this column, checking both original and new names
        spec <- NULL
        if (col %in% names(col_specs)) {
          spec <- col_specs[[col]]
        } else {
          # Check if this column was renamed from another column
          for (spec_name in names(col_specs)) {
            if (!is.null(col_specs[[spec_name]]$new_name) &&
                col_specs[[spec_name]]$new_name == col) {
              spec <- col_specs[[spec_name]]
              break
            }
          }
        }

        if (!is.null(spec) && spec$type == "factor") {
          ordered_status <- ifelse(is.ordered(combined_data[[col]]),
                                   "ordered", "unordered")
          message(sprintf(
            "- %s (%s, %d levels: %s)",
            col,
            ordered_status,
            nlevels(combined_data[[col]]),
            paste(levels(combined_data[[col]]), collapse = ", ")
          ))
        } else {
          message("- ", col, " (auto-converted to factor)")
        }
      }
    }
  }

  return(combined_data)
}


read_agri_dataset <- function(data_dir,
                              dataset_type,
                              col_specs,
                              file_pattern = NULL,
                              ordered_factors = FALSE,
                              id_var = "PlantletID",
                              verbose = TRUE,
                              ...) {
  # Load required packages
  require(data.table)

  # Input validation
  if (!dir.exists(data_dir)) {
    stop("Directory does not exist: ", data_dir)
  }

  if (missing(dataset_type) || !is.character(dataset_type)) {
    stop("dataset_type must be a character string")
  }

  if (missing(col_specs) || !is.list(col_specs)) {
    stop("col_specs must be a named list of column specifications")
  }

  # Set default file pattern if not provided
  if (is.null(file_pattern)) {
    file_pattern <- sprintf("*%s*.csv$", dataset_type)
  }

  # Find matching files
  file_paths <- list.files(
    path = data_dir,
    pattern = file_pattern,
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(file_paths) == 0) {
    warning("No files found matching pattern: ", file_pattern)
    return(data.table())
  }

  # Process each file
  result_list <- lapply(file_paths, function(fp) {
    if (verbose) message("Processing: ", basename(fp))

    # Read data
    dt <- data.table::fread(fp, ...)

    # Convert to data.table if not already
    if (!data.table::is.data.table(dt)) {
      dt <- data.table::as.data.table(dt)
    }

    # Apply column specifications
    for (col_name in names(col_specs)) {
      spec <- col_specs[[col_name]]
      target_name <- if (!is.null(spec$new_name)) spec$new_name else col_name

      if (col_name %in% names(dt)) {
        # Rename if needed
        if (!is.null(spec$new_name)) {
          data.table::setnames(dt, col_name, target_name)
        }

        # Apply type conversion
        switch(
          spec$type,
          "character" = dt[, (target_name) := as.character(get(target_name))],
          "numeric" = dt[, (target_name) := as.numeric(get(target_name))],
          "integer" = dt[, (target_name) := as.integer(get(target_name))],
          "Date" = {
            origin <- if (!is.null(spec$origin)) spec$origin else "1970-01-01"
            dt[, (target_name) := as.Date(get(target_name), origin = origin)]
          },
          "factor" = {
            # Handle factor creation with optional levels and ordering
            col_data <- dt[[target_name]]

            # Get levels if specified, otherwise use sorted unique values
            levels <- if (!is.null(spec$levels)) {
              spec$levels
            } else {
              if (is.factor(col_data)) {
                levels(col_data)
              } else {
                sort(unique(col_data[!is.na(col_data)]))
              }
            }

            # Determine if ordered
            ordered <- if (!is.null(spec$ordered)) spec$ordered else ordered_factors

            # Create factor
            dt[, (target_name) := factor(
              get(target_name),
              levels = levels,
              ordered = ordered
            )]
          },
          if (verbose) warning("Unknown type specification: ", spec$type)
        )
      }
    }

    # Add metadata
    # dt[, `:=`(
    #   source_file = basename(fp),
    #   processing_date = Sys.Date(),
    #   dataset_type = dataset_type
    # )]

    return(dt)
  })

  # Combine all files
  combined_data <- plyr::rbind.fill(result_list)

  if (nrow(combined_data) == 0) {
    warning("No data found after processing files")
    return(combined_data)
  }

  # Check for ID variable presence
  if (!id_var %in% names(combined_data)) {
    warning("ID variable '", id_var, "' not found in dataset")
    return(combined_data)
  }

  # Handle duplicates based on ID variable
  if (anyDuplicated(combined_data[[id_var]])) {
    if (verbose) message("\nProcessing duplicates based on ", id_var)

    # Ensure we're working with a data.table
    if (!data.table::is.data.table(combined_data)) {
      combined_data <- data.table::as.data.table(combined_data)
    }

    # Calculate number of missing values per row
    cols_to_check <- setdiff(names(combined_data),
                             c(id_var, "source_file", "processing_date", "dataset_type"))
    combined_data[, missing_count := apply(is.na(.SD), 1, sum), .SDcols = cols_to_check]

    # Order by ID and missing count (fewest missing first)
    data.table::setorderv(combined_data, c(id_var, "missing_count"))

    # Keep first occurrence (with fewest missing values)
    combined_data <- combined_data[!duplicated(combined_data[[id_var]]), ]

    # Remove temporary column
    combined_data[, missing_count := NULL]

    if (verbose) {
      dup_count <- sum(duplicated(combined_data[[id_var]]))
      message("Removed ", dup_count, " duplicate ", id_var, "s, keeping rows with fewest missing values")
    }
  }

  if (verbose) {
    message(sprintf(
      "\nFinal dataset: %d rows with %d unique %s values",
      nrow(combined_data),
      data.table::uniqueN(combined_data[[id_var]]),
      id_var
    ))

    # Report on factor columns
    factor_cols <- names(which(sapply(combined_data, is.factor)))
    if (length(factor_cols) > 0) {
      message("\nFactor columns created:")
      for (col in factor_cols) {
        # Find the spec for this column, checking both original and new names
        spec <- NULL
        if (col %in% names(col_specs)) {
          spec <- col_specs[[col]]
        } else {
          # Check if this column was renamed from another column
          for (spec_name in names(col_specs)) {
            if (!is.null(col_specs[[spec_name]]$new_name) &&
                col_specs[[spec_name]]$new_name == col) {
              spec <- col_specs[[spec_name]]
              break
            }
          }
        }

        if (!is.null(spec) && spec$type == "factor") {
          ordered_status <- ifelse(is.ordered(combined_data[[col]]),
                                   "ordered", "unordered")
          message(sprintf(
            "- %s (%s, %d levels: %s)",
            col,
            ordered_status,
            nlevels(combined_data[[col]]),
            paste(levels(combined_data[[col]]), collapse = ", ")
          ))
        } else {
          message("- ", col, " (auto-converted to factor)")
        }
      }
    }
  }

  return(combined_data)
}


process_data <- function(df, columns = NULL, data_type, filter_conditions = list()) {
  # Select columns if specified
  if (!is.null(columns)) {
    df <- df[, columns, drop = FALSE]
  }

  # Apply clean_genotype_columns function
  processed_df <- df |>
    # clean_genotype_columns() |>
    dplyr::select(Location, Crossnumber, Female_Genotype, Male_Genotype, dplyr::everything())

  # Build filter expressions based on filter_conditions
  if (length(filter_conditions) > 0) {
    for (col in names(filter_conditions)) {
      value <- filter_conditions[[col]]

      # Check if column exists
      if (!col %in% names(processed_df)) {
        warning(paste("Column", col, "not found in the dataframe. Skipping this filter."))
        next
      }

      # Filter for non-NA values
      processed_df <- processed_df |>
        dplyr::filter(!is.na(!!rlang::sym(col)))

      # If a numeric threshold is provided, filter for values > threshold
      if (!is.null(value) && is.numeric(value)) {
        processed_df <- processed_df |>
          dplyr::filter(!!rlang::sym(col) > value)
      }
    }
  }

  # Save the complete dataset
  saveRDS(processed_df, data_path(paste0("all_", data_type, ".rds")))

  # Save by location (if Location column exists)
  if ("Location" %in% names(processed_df)) {
    processed_df |>
      dplyr::group_split(Location) |>
      purrr::walk(~ saveRDS(.x, data_path(paste0(tolower(unique(.x$Location)), "_", data_type, ".rds"))))
  } else {
    warning("Location column not found. Cannot split by location.")
  }

  # Return the processed dataframe
  invisible(processed_df)
}

process_with_plantlets <- function(df, all_plantlets, join_columns, data_type, filter_conditions = list()) {
  # Input validation
  if (!all(join_columns %in% names(all_plantlets))) {
    missing_cols <- join_columns[!join_columns %in% names(all_plantlets)]
    stop(paste("Column(s)", paste(missing_cols, collapse = ", "),
               "not found in the plantlets dataframe"))
  }

  # Join with plantlet data
  processed_df <- df |>
    dplyr::left_join(all_plantlets[, ..join_columns, drop = FALSE]) |>
    #clean_genotype_columns() |>  # remove url from genotypes
    dplyr::select(Location, PlantletID, Female_Genotype, Male_Genotype, dplyr::everything())

  # Build filter expressions based on filter_conditions
  if (length(filter_conditions) > 0) {
    for (col in names(filter_conditions)) {
      value <- filter_conditions[[col]]

      # Check if column exists
      if (!col %in% names(processed_df)) {
        warning(paste("Column", col, "not found in the dataframe. Skipping this filter."))
        next
      }

      # Filter for non-NA values
      processed_df <- processed_df |> dplyr::filter(!is.na(!!rlang::sym(col)))

      # If a numeric threshold is provided, filter for values > threshold
      if (!is.null(value) && is.numeric(value)) {
        processed_df <- processed_df |>
          dplyr::filter(!!rlang::sym(col) > value)
      }
    }
  }

  # Save the complete dataset
  saveRDS(processed_df, data_path(paste0("all_", data_type, ".rds")))

  # Save by location (if Location column exists)
  if ("Location" %in% names(processed_df)) {
    processed_df |>
      dplyr::group_split(Location) |>
      purrr::walk(~ saveRDS(.x, data_path(paste0(tolower(unique(.x$Location)), "_", data_type, ".rds"))))
  } else {
    warning("Location column not found. Cannot split by location.")
  }

  # Return the processed dataframe
  invisible(processed_df)
}

