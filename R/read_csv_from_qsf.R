#' Read Qualtrics CSV Files and Clean Column Names
#'
#' This function reads a CSV file exported from Qualtrics, extracts the variable names, 
#' skips the metadata, and returns a cleaned data frame with standardized column names.
#'
#' @param file_location A string indicating the path to the CSV file to be read.
#'
#' @return A data frame with cleaned column names and without Qualtrics metadata.
#' 
#' @details 
#' The function performs the following steps:
#' 1. Reads the CSV file without assuming any column names.
#' 2. Extracts the variable names from the first row of the read data.
#' 3. Re-reads the data, skipping the first three rows (Qualtrics metadata) and uses the extracted variable names.
#' 4. Cleans the variable names using the `janitor::clean_names` function.
#'
#' @examples 
#' # Ensure you have set your working directory or provide an absolute path
#' data <- read_csv_from_qsf("your_qualtrics_file.csv")
#'
#' @importFrom dplyr %>%
#' @importFrom readr read_csv
#' @importFrom janitor clean_names
#'
read_csv_from_qsf <- function(file_location) {
  
  # Read the CSV without column names
  data <- read_csv(file = file_location,
                   na = ".",
                   col_names = FALSE,
                   show_col_types = FALSE)
  
  # Extract variable names
  var_names <- as.character(data[1, ])
  
  # Read the data, skipping metadata and using extracted variable names
  data <- read_csv(file = file_location,
                   na = ".",
                   skip = 3,
                   col_names = var_names,
                   show_col_types = FALSE)
  
  # Clean the variable names
  data <- janitor::clean_names(data)
  
  return(data)
}
