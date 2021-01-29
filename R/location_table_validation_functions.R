## -------------------------------------------------------------------------------------->
##
## Functions for validating location tables used in CRVS analysis.
##
## -------------------------------------------------------------------------------------->


#' Get valid CRVS location tables
#'
#' @description Helper function that returns valid location table names for CRVS analysis.
#'   Note: This function is not exposed to the package user.
#'
#' @return Character vector of valid table names
#'
#' @export
get_valid_location_table_names <- function(){
  valid_tables <- c('snapshot','annual','change','stable','full')
  return(valid_tables)
}


#' Check that a CRVS location table name is valid
#'
#' @description Helper function to check that a CRVS location table name is valid. Note:
#'   This function is not exposed to the package user.
#'
#' @param table_name Character vector of length 1 containing one valid table name
#'
#' @return Returns invisibly. Informatively fails if the table name is not valid.
validate_location_table_name <- function(table_name){
  valid_tables <- get_valid_location_table_names()
  if(length(table_name) != 1){
    stop("Exactly one table name must be passed at a time.")
  }
  if(!table_name %in% valid_tables){
    stop(
      'Table name "', table_name, '" not valid. Valid names include: ',
      paste(valid_tables, collapse=', ')
    )
  }
  invisible()
}


#' Get CRVS location table field names
#'
#' @description Helper function that returns that names of all required fields for each
#'   type of CRVS location table. Note: this function is not exposed to the package user.
#'
#' @param table_name [char] Name of a valid location table
#'
#' @return Character vector containing all required field names for a given location table
get_location_table_required_fields <- function(table_name){
  # Check validity
  validate_location_table_name(table_name)
  # List of all required fields by table
  required_fields <- list(
    snapshot = c(
      'iso','year','level','adm_code','parent_code','adm_name','adm_ascii_name'
    ),
    annual = c(
      'iso','year','level','adm_code','parent_code','adm_name','adm_ascii_name'
    ),
    change = c(
      'iso','year','change_number','change_type','level','start_code','end_code',
      'swap_with'
    ),
    stable = c(
      'iso', 'level', 'stable_id', 'adm_code', 'start_year', 'end_year'
    ),
    full = c(
      'iso','year','level','stable_id','adm_code','parent_code','adm_name','adm_ascii_name'
    )
  )
  # Return valid fields for this particular table
  return(required_fields[[table_name]])
}


#' Get CRVS location table field classes
#'
#' @description Helper function that returns the classes (integer, character, and so on)
#'   expected for each field in a given location table. Note: This function is not exposed
#'   to the package user.
#'
#' @param table_name [char] Name of a valid location table
#'
#' @return Named character vector listing expected class for each required column in
#'   a CRVS location table.
get_location_table_field_classes <- function(table_name){
  # Get the table's required fields
  required_fields <- get_location_table_required_fields(table_name)
  # Set up column classes in the same format used by the `colClasses` argument to
  #  `read.csv()` and `data.table::fread()`
  char <- 'character'
  int <- 'integer'
  field_classes <- c(
    iso=char, year=int, level=int, adm_code=char, parent_code=char, adm_name=char,
    adm_ascii_name=char, change_number=int, change_type=char, start_code=char,
    end_code=char, swap_with=char
  )
  # Check that all required field names are included in the classes vector
  missing_fields <- setdiff(required_fields, names(field_classes))
  if(length(missing_fields) > 0){
    stop(
      "Missing required field classes for table ", table_name, ": ",
      paste0(missing_fields, collapse=', ')
    )
  }
  # Return the subset of column classes that appear in the vector of required fields
  return(field_classes[required_fields])
}


#' Validate location hierarchy table
#'
#' @description Validate that a location hierarchy table is internally consistent for
#'   each queried year.
#'
#' @details This function can be used to check the 'snapshot' and 'annual' location
#'   tables. It performs the following checks:
#'   - All required columns are present and have the correct data types and content
#'   - All required location levels are present
#'   - For multi-level tables, each location at the more-detailed levels have valid
#'       parents and each location at the less-detailed levels have valid children
#'
#' @param input_table An input data.table containing a 'snapshot' or 'annual' location
#'   hierarchy table
#' @param check_years [integer] Which years should be checked?
#'
#' @return Returns a list of internal validity issues. If there are no internal validity
#'   issues, returns an empty list.
#'
#' @import data.table
validate_location_hierarchy_table_internal <- function(
  input_table, check_years
){
  issues_list <- list()
  return(issues_list)
}


#' Validate 'snapshot' location hierarchy table
#'
#' @description Wrapper function to validate the location snapshot table, which is a
#'   location hierarchy table that does not need to have all consecutive years available.
#'
#' @param input_table An input data.table containing a 'snapshot' or 'annual' location
#'   hierarchy table
#' @param check_years [integer] Which years should be checked?
#'
#' @return Returns a list of internal validity issues. If there are no internal validity
#'   issues, returns an empty list.
#'
#' @import data.table
validate_location_snapshot_table_internal <- function(
  input_table, check_years
){
  issues_list <- validate_location_hierarchy_table_internal(
    input_table = input_table, check_years = check_years
  )
  return(issues_list)
}


#' Validate 'annual' location hierarchy table
#'
#' @description Wrapper function to validate the annual location hierarchy table - this is
#'   identical to the 'snapshot' table, with the exception that it must include all
#'   sequential years in a time series.
#'
#' @param input_table An input data.table containing a 'snapshot' or 'annual' location
#'   hierarchy table
#' @param check_years [integer] Which years should be checked?
#'
#' @return Returns a list of internal validity issues. If there are no internal validity
#'   issues, returns an empty list.
#'
#' @import data.table
validate_location_annual_table_internal <- function(
  input_table, check_years
){
  issues_list <- list()
  # Check for sequential years
  seq_years <- min(check_years):max(check_years)
  missing_years <- setdiff(seq_years, unique(input_table$year))
  if(length(missing_years) > 0){
    issues_list <- c(
      issues_list,
      list(paste("Sequential years missing in time range: ",missing_years,collapse=', '))
    )
  }
  # Check for all other location hierarchy issues
  issues_list <- c(
    issues_list,
    validate_location_hierarchy_table_internal(
      input_table = input_table, check_years = check_years
    )
  )
  # Return
  return(issues_list)
}


#' Validate location change table
#'
#' @description Validate that a location change table is internally consistent for each
#'   queried year.
#'
#' @details This function can be used to check the 'change' location table. It performs
#'   the following checks:
#'   - All required columns are present and have the correct data types and content
#'   - Each change type is correctly formatted
#'   - The changes are numbered sequentially, either overall or by year
#'   - For border changes, 'swap_with' fields are reciprocal (if A<->B then B<->A)
#'
#' @param input_table An input data.table for the 'change' location table
#' @param check_years [integer] Which years should be checked?
#'
#' @return Returns a list of internal validity issues. If there are no internal validity
#'   issues, returns an empty list.
#'
#' @import data.table
validate_location_change_table_internal <- function(
  input_table, check_years
){
  issues_list <- list()
  return(issues_list)
}


#' Validate stable location hierarchy table
#'
#' @description Validate that a stable location hierarchy table is internally consistent
#'
#' @param input_table An input data.table for the 'stable' location table, created using
#'   the `TODO()` function.
#'
#' @return Returns a list of internal validity issues. If there are no internal validity
#'   issues, returns an empty list.
#'
#' @import data.table
validate_location_stable_table_internal <- function(
  input_table
){
  issues_list <- list()
  return(issues_list)
}


#' Validate full location lookup table
#'
#' @description Validate that a full location lookup table is internally consistent
#'
#' @param input_table An input data.table for the 'full' location table, created using
#'   the `TODO()` function.
#'
#' @return Returns a list of internal validity issues. If there are no internal validity
#'   issues, returns an empty list.
#'
#' @import data.table
validate_location_full_table_internal <- function(
  input_table
){
  issues_list <- list()
  return(issues_list)
}


#' Validate any location hierarchy table
#'
#' @description Validate the internal validity of any location table used for CRVS
#'   spatial analysis
#'
#' @param input_table The location table to be validated for internal consistency
#' @param table_name Name of the type of location table being validated. Must be a valid
#'   table name per `get_valid_location_table_names()`.
#' @param check_years [integer, default NULL] Which years of data should be checked? Only
#'   used for the 'snapshot', 'annual', and 'change' table types. For these table types,
#'   `check_years` is NULL (the default), checks all unique years listed in the table.
#' @param raise_issues_as_errors [logical, default TRUE] Should the internal validity
#'   issues be raised as errors? If TRUE (the default), this function checks for all
#'   issues and then errors if any were found. If FALSE, this function returns a list of
#'   issues and prints a warning message if any were found.
#'
#' @return If no issues were found, returns an empty list. If issues were found and
#'   `raise_issues_as_errors` is TRUE, throws an error. If issues were found and
#'   `raise_issues_as_errors` is FALSE, returns a list of issues.
#'
#' @import data.table knitr
#' @export
validate_location_table <- function(
  input_table, table_name, check_years = NULL, raise_issues_as_errors = TRUE
){
  # Check that table is valid and that all required columns are present
  validate_location_table_name(table_name)
  field_classes <- get_location_table_field_classes(table_name)
  missing_fields <- setdiff(names(input_table), names(field_classes))
  if(length(missing_fields) > 0){
    stop(
      "Pre-validation error: missing required fields ",
      paste(missing_fields, collapse=', ')
    )
  }

  # Check that table fields all have the correct classes
  class_mismatch_table <- data.table(
    field = names(field_classes),
    required_class = field_classes
  )[, class_in_data := class(input_table[[field]])][ required_class != class_in_data , ]
  if(nrow(class_mismatch_table) > 0){
    stop(
      "Pre-validation error: Incorrect classes for required fields:\n",
      paste(capture.output(class_mismatch_table), collapse='\n')
    )
  }

  # Define list of years for particular tables if `check_years` is NULL
  if(is.null(check_years) & (table_name %in% c('snapshot', 'change'))){
    check_years <- na.omit(sort(unique(input_table$year)))
  } else if(is.null(check_years) & (table_name == 'annual')){
    check_years <- seq(
      min(input_table$year, na.rm=T), max(input_table$year, na.rm=T), by=1
    )
  }

  # Apply specific validation function for this table
  validation_function <- paste0('validate_location_',input_table,'_table_internal')
  issues_list <- do.call(
    validation_function,
    args = list(
      input_table = input_table,
      check_years = check_years
    )[names(formals(validation_function))]
  )

  # Optionally raise errors
  if((length(issues_list) > 0) & (raise_issues_as_errors==TRUE)){
    stop(
      "Validation issues for table ",table_name,":\n  - ",
      paste(issues_list, collapse='\n  - ')
    )
  }
  # If validity issues were not raised as errors, return them as a list
  return(issues_list)
}



#' Validate consistency between location hierarchies and location changes
#'
#' @description Validate the cross-table consistency between location hierarchies IN
#'   particular years and location change tables representing differences ACROSS multiple
#'   years.
#'
#' @details This function takes two tables: a location hierarchy table (either the
#'   'snapshot' or 'annual' table) and a location change table. These tables should have
#'   already been validated for internal consistency using the functions
#'   `validate_location_hierarchy_table_internal()` and
#'   `validate_location_change_table_internal()`. This function checks that the two tables
#'   are consistent across all specified years.
#'
#' @param hierarchy_table An input data.table for the 'snapshot' or 'annual' location
#'   table
#' @param change_table An input data.table for the 'change' location table
#' @param max_level [integer] how many levels of administrative subdivisions should be
#'   checked?
#' @param check_years [integer] Which years should be checked?
#'
#' @return Returns a list of internal validity issues. If there are no internal validity
#'   issues, returns an empty list.
#'
#' @import data.table
#' @export
validate_location_hierarchy_change_consistent <- function(
  hierarchy_table, change_table, max_level, check_years
){
  issues_list <- list()
  return(issues_list)
}
