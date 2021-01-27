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
get_valid_location_table_names <- function(){
  valid_tables <- c('snapshot','change')
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
    change = c(
      'iso','year','change_number','change_type','level','start_code','end_code',
      'swap_with'
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
