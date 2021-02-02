## -------------------------------------------------------------------------------------->
##
## Functions for CRVS file reading and writing
##
## -------------------------------------------------------------------------------------->


#' Load location table from CSV
#'
#' @description Load a location table for CRVS mapping from a CSV file
#'
#' @details This function ensures that the class of each required column is the correct
#'   type, but does not otherwise validate the table.
#'
#' @param table_name [char] Name of the CRVS table that is being loaded
#' @param file [char] Filepath to read from
#' @param ... Other arguments to data.table::fread()
#'
#' @return Data.table containing the table with properly formatted columns, where known
#'
#' @import data.table
#' @export
load_location_table <- function(table_name, file, ...){
  # Get possible location table columns and data types
  colClasses <- mapcrvs::get_location_table_field_classes(table_name)
  out_table <- data.table::fread(file = file, colClasses = colClasses, ...)
  return(out_table)
}
