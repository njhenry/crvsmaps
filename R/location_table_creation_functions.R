## -------------------------------------------------------------------------------------->
##
## Functions for creating tables used to define location sets and hierarchies for spatial
## CRVS analysis. These tables include:
##  - Location hierarchies for each year over a time period
##  - Location change histories (blank templates, to be filled by user)
##  - Stable location hierarchy tables
##  - Full location metadata tables
##
## -------------------------------------------------------------------------------------->


#' Create a blank template for a location table
#'
#' @description Create a blank location hierarchy table with the appropriate fields and
#'   column types. This function can be used to confirm the proper format for the location
#'   hierarchy snapshot and change history tables that need to be filled by the user.
#'
#' @details For more information about the location tables, refer to the package vignettes
#'   and the documentation for \code{\link{validate_location_table}}
#'
#' @param table_name [char] Name of a valid location table. To check valid location table
#'   names, pull from `get_valid_location_table_names()`
#'
#' @return A data.table with a single row of NA data entries, formatted with the correct
#'  field names and data types for the specified location table.
#'
#' @import data.table
#' @export
create_location_table_template <- function(table_name){
  # Check table name is valid
  validate_location_table_name(table_name)
  # Get location table required fields and classes
  field_classes <- get_location_table_field_classes(table_name)
  # Create data.table
  input_string <- paste0(
    paste(names(field_classes), collapse=','),
    '\n',
    paste(rep("NA", length(field_classes)), collapse=',')
  )
  template_table <- data.table::fread(
    input = input_string,
    colClasses = field_classes
  )
  return(template_table)
}


#' Build stable location table
#'
#' @description Construct a table that identifies unique geographic units with stable
#'   boundaries throughout the time period.
#'
#' @details For more information about the 'change', 'annual', and 'stable' location
#'   tables, refer to the package vignettes and the documentation for
#'   \code{\link{validate_location_table}}
#'
#' @param annual_table A valid 'annual' location table describing the location
#'   hierarchy for all years in the years between \code{year_start} and \code{year_end}
#'   (inclusive).
#' @param change_table A valid 'change' location table describing changes to the location
#'   hierarchy from one year to the next.
#' @param year_start First year of the analysis time series
#' @param year_end Final year of the analysis time series
#' @param verbose [bool, default TRUE] Print messages about table construction progress?
#'
#' @return
#'
#' @import data.table
#' @export
build_stable_location_table <- function(
  annual_table, change_table, year_start, year_end, verbose = TRUE
){
  if(verbose) message("Building stable location table:")

  # Validate input arguments
  if(verbose) message("  - Validating input arguments")
  validate_year_range(year_start = year_start, year_end = year_end)
  year_range <- start_year:end_year
  validate_location_table('change', input_table = change_table, check_years = year_range)
  validate_location_table('annual', input_table = annual_table, check_years = year_range)

  # ETC ETC
  if(verbose) message("  - ETC ETC")


  return(data.table())
}


#' Build full location metadata table
#'
#' @description
#'
#' @details For more information about the 'annual', 'stable', and 'full' location tables,
#'   refer to the package vignettes and the documentation for
#'   \code{\link{validate_location_table}}
#'
#' @param annual_table A valid 'annual' location table describing the location
#'   hierarchy for all years in the years between \code{year_start} and \code{year_end}
#'   (inclusive).
#' @param stable_table A valid 'stable' location table that identifies unique geographic
#'   units that have stable boundaries across the time period.
#' @param year_start First year of the analysis time series
#' @param year_end Final year of the analysis time series
#' @param verbose [bool, default TRUE] Print messages about table construction progress?
#'
#' @return
#'
#' @import data.table
#' @export
build_full_location_table <- function(
  annual_table, stable_table, year_start, year_end, verbose = TRUE
){
  if(verbose) message("Building full location metadata table:")

  # Validate input arguments
  if(verbose) message("  - Validating input arguments")
  validate_year_range(year_start = year_start, year_end = year_end)
  year_range <- start_year:end_year
  validate_location_table('annual', input_table = annual_table, check_years = year_range)
  validate_location_table('stable', input_table = stable_table, check_years = year_range)

  # ETC ETC
  if(verbose) message("  - ETC ETC")

  return(data.table())
}


#' Build the 'stable' and 'full' location tables.
#'
#' @description Based on two user-constructed location tables ('annual' and 'change'),
#'   construct two derivative location tables that are needed for analysis.
#'
#' @details The location tables constructed by this function are, in order, 'stable' and
#'   'full'. For more information on all the location tables, refer to the package
#'   vignettes and the documentation for \code{\link{validate_location_table}}
#'
#' @param annual_table A valid 'annual' location table describing the location hierarchy
#'   in years where data is available.
#' @param change_table A valid 'change' location table describing changes to the location
#'   hierarchy from one year to the next.
#' @param year_start First year of the analysis time series
#' @param year_end Final year of the analysis time series
#' @param verbose [bool, default TRUE] Print messages about table construction progress?
#'
#' @return A list of the three derived location tables for small-area CRVS analysis:
#'    - 'stable': Identifies geographic units with stable boundaries over the time period
#'    - 'full': Full location metadata table containing identifiers from both the 'annual'
#'        and 'stable' tables
#'
#' @export
build_all_location_tables <- function(
  annual_table, change_table, year_start, year_end, verbose = TRUE
){
  # Using annual and change tables, build stable table
  stable_table <- build_stable_location_table(
    annual_table = annual_table, change_table = change_table, year_start = year_start,
    year_end = year_end, verbose = verbose
  )
  validate_location_table(
    'stable', input_table=stable_table, check_years=year_start:year_end
  )

  # Merge stable table onto annual table to build full location metadata table
  full_table <- build_full_location_table(
    annual_table = annual_table, stable_table = stable_table, year_start = year_start,
    year_end = year_end, verbose = verbose
  )
  validate_location_table('full', input_table=full_table, check_years=year_start:year_end)

  # Return
  if(verbose) message("All location tables have been constructed successfully.")
  return(list(stable = stable_table, full = full_table))
}
