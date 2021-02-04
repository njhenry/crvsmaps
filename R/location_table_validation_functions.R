## -------------------------------------------------------------------------------------->
##
## Functions for validating location tables used in CRVS analysis.
##
## -------------------------------------------------------------------------------------->


#' Validate analysis years
#'
#' @description Validate the starting and ending years for the analysis period
#'
#' @details Note: this is a helper function that is not exposed to the user
#'
#' @param year_start First year of the analysis time series
#' @param year_end Final year of the analysis time series
#'
#' @return Returns silently; fails informatively if the year range is invalid
validate_year_range <- function(year_start, year_end){
  # Validate years
  if((class(year_start) != 'integer')) stop("year_start must be an integer.")
  if((class(year_end) != 'integer')) stop("year_end must be an integer.")
  if(year_start > year_end) stop('year_start must be less than or equal to year_end')
  invisible()
}


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
  # Initialize issues lists
  il <- list()
  # Check that there are no NA values in any fields except for 'parent_code'
  reqd_cols <- setdiff(get_location_table_required_fields('snapshot'), 'parent_code')
  for(reqd_col in reqd_cols){
    if(any(is.na(input_table[[reqd_col]]))) il <- c(il, paste0('NAs in "',reqd_col,'" field.'))
  }
  # Check for missing years
  missing_yrs <- setdiff(check_years, unique(input_table$year))
  if(length(missing_yrs) > 0){
    il <- c(il, paste("Missing required years: ", missing_yrs ,collapse=', '))
  }

  # Iterate by level and year
  max_adm = max(input_table$level)

  for(thisyr in check_years){
    for(thislvl in 1:max_adm){
      # Helper function
      add_iss <- function(...) paste0('Year ', thisyr, ', admin', thislvl, ': ', ...)
      # Check subset of dataset
      in_sub <- input_table[(year==thisyr) & (level==thislvl), ]

      # Only perform most checks if the data subset has any rows
      if(nrow(in_sub) == 0){
        il <- c(il, add_iss("Missing all rows in this required subset!"))
      } else {

        # Check that there are no duplicate locations in a given year-level
        dupes <- in_sub[, .N, by=adm_code][N > 1, adm_code]
        if(length(dupes) > 0){
          il <- c(il, add_iss("Duplicate location codes ", paste(dupes, collapse=', ')))
        }

        # If this is not the most detailed administrative level, check that all locations
        #  have children in the next-most-detailed level
        if(thislvl < max_adm){
          child_codes <- input_table[
            (year==thisyr) & (level==(thislvl+1)) & !is.na(parent_code),
            unique(parent_code)
          ]
          missing_children <- na.omit(setdiff(unique(in_sub$adm_code), child_codes))
          if(length(missing_children) > 0){
            il <- c(il, add_iss(
              "Locations ", paste(missing_children, collapse=', '),
              "missing children in level ",thislvl + 1
            ))
          }
        }

        # For admin levels > 1 only, check that parent_code matches real locations
        if(thislvl > 1){
          # Check for NA parents
          na_parents <- nrow(in_sub[is.na(parent_code), ])
          if(na_parents > 0) il <- c(il, add_iss(na_parents, " rows have NA parent_codes."))
          # Check for parents that are not recorded in the level above
          parent_codes <- input_table[
            (year==thisyr) & (level==(thislvl-1)) & !is.na(adm_code),
            unique(adm_code)
          ]
          missing_parents <- na.omit(setdiff(unique(in_sub$parent_code), parent_codes))
          if(length(missing_parents) > 0){
            il <- c(il, add_iss(
              "Parent codes ", paste(missing_parents, collapse=', '),
              " not found among admin",thislvl-1," locations"
            ))
          }
        }

      } # END else statement indicating data subset has > 0 rows
    } # END admin level iteration
  } # END year iteration

  return(il)
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


#' Get valid change types
#'
#' @description This function enumerates all the valid location change types
#'
#' @return [char] Vector of valid change types
#'
#' @export
get_valid_change_types <- function(){
  return(c('SPLIT','MERGE','CODE','BORDER'))
}


#' Validate a single location change
#'
#' @description Validate a single change within a location change table
#'
#' @details This is a helper function for
#'   \code{\link{validate_location_change_table_internal}} and is not exposed to the
#'   package user. This function does NOT check field names or data types, since these
#'   checks are assumed to happen at the table level.
#'
#' @param change_table A data.table subset of the 'change' location table, providing all
#'   available rows corresponding to a single location change in a single year
#'
#' @return List of issues corresponding to this particular change
#'
#' @import data.table
validate_single_location_change <- function(change_table){
  # Check that there is only one valid change type represented
  change_type <- unique(change_table$change_type)
  if(length(change_type) != 1){
    return("This change does not have a consistent change type across all rows")
  }
  valid_change_types <- get_valid_change_types()
  if(!change_type %in% valid_change_types){
    valid_changes_txt <- paste(valid_change_types, collapse=', ')
    return(paste0("change_type not one of ", valid_changes_txt, " (case-sensitive)"))
  }

  # Conditional on the change type being valid, perform more detailed checks
  il <- character(0)

  n_rows <- nrow(change_table)
  n_start <- length(unique(change_table$start_code))
  n_end <- length(unique(change_table$end_code))

  if(change_type == 'SPLIT'){
    if(n_rows < 2) il <- c(il, 'SPLITs must have at least two rows')
    if(n_start > 1) il <- c(il, 'All start_codes for SPLIT change must be identical')
    if(n_end < n_rows) il <- c(il, 'All end_codes for SPLIT change must be unique')

  } else if(change_type == 'MERGE'){
    if(n_rows < 2) il <- c(il, 'MERGEs must have at least two rows')
    if(n_start < n_rows) il <- c(il, 'All start_codes for MERGE change must be unique')
    if(n_end > 1) il <- c(il, 'All end_codes for MERGE change must be identical')

  } else if(change_type == 'BORDER'){
    if(n_rows != 2) il <- c(il, "BORDER swaps must have at exactly two rows")
    if(n_start != 2) il <- c(il, "BORDER swaps must start with two unique locations")
    if(n_end != 2) il <- c(il, "BORDER swaps must end with two unique locations")
    start_codes <- unique(change_table$start_code)
    swap_codes <- unique(change_table$swap_with)
    if(any(is.na(swap_codes))) il <- c(il, 'swap_with must be filled for BORDER swaps')
    if(any(start_codes==swap_codes)) il <- c(il, 'swap_with cannot be the same as start_code')
    if(any(sort(start_codes) != sort(swap_codes))){
      il <- c(il, 'all start_codes must swap with another territory in a BORDER change')
    }

  } else if (change_type == 'CODE'){
    if(n_rows != 1) il <- c(il, 'CODE changes should have exactly one row')
    if(any(change_type$start_code == change_type$end_code)){
      il <- c(il, "CODE changes should have different start and end codes")
    }

  } else {
    stop('Validation for change type ', change_type, ' not initialized!')
  }

  return(il)
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
  il <- list()

  # Check for NAs
  reqd_cols <- setdiff(get_location_table_required_fields('change'), c('swap_with'))
  for(reqd_col in reqd_cols){
    if(any(is.na(input_table[[reqd_col]]))) il <- c(il, paste("NAs in field", reqd_col))
  }

  # Validate each change separately
  in_sub <- input_table[(year %in% check_years) & !is.na(level) & !is.na(change_number), ]
  change_wise_issues_dt <- in_sub[,
    .(issue_txt = validate_single_location_change(.SD)),
    by=.(year, level, change_number)
  ]
  if(nrow(change_wise_issues_dt) > 0){
    change_wise_issues <- change_wise_issues_dt[,
      paste0('Year ',year,', level ',level,', change ',change_number,': ',issue_txt)
    ]
    il <- c(il, as.list(change_wise_issues))
  }

  return(il)
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
#' @details To create a valid location hierarchy over a period of time, the user needs to
#'   create two tables representing that location hierarchy, one representing snapshots of
#'   the location hierarchy in particular years, and the other representing changes to the
#'   location hierarchy across years. These tables are validated against each other and
#'   then used to construct three additional useful location tables for CRVS analysis and
#'   modeling. For more details and examples of these tables, check the package vignettes.
#'
#    The 'snapshot' table includes details about all sub-national administrative units in
#'   years where the user has location data available. This table must include a complete
#'   accounting of all administrative levels from the top level (states or provinces) down
#'   to the most detailed unit of analysis (for example, districts or cantons) for every
#'   year where data is available. The user is required to include data from the first and
#'   last year of the analysis time series.
#'
#'   REQUIRED FIELDS FOR THE 'snapshot' TABLE:
#'   - 'iso': ISO3 code for the country being modeled (character)
#'   - 'year': Four-digit year when a snapshot was available (integer)
#'   - 'level': Standardized administrative level for each location. Top-level
#'       administrative divisions, often called states or provinces, are level 1. The
#'       subdivisions contained within each top-level division, often called counties or
#'       districts, are level 2. The subdivisions within each of those are level 3, and
#'       so on. The top-level country information (level 0) should NOT be included in this
#'       table. (integer)
#'   - 'adm_code': Uniquely identifying code for each administrative subdivision in the
#'       year when the snapshot was taken. These codes will often be numeric, but
#'       internally they are treated as strings to catch alphanumeric codes. (character)
#'   - 'parent_code': Uniquely identifying code for the containing/parent location of a
#'       given administrative subdivision. These should be internally consistent for each
#'       snapshot year - for example, the parent codes of level 2 locations should match
#'       identifying codes for level 1 locations. The parent codes for level 1 locations
#'       should be input as NA strings. (character)
#'   - 'adm_name': Name of the administrative subdivision. (character)
#'   - 'adm_ascii_name': Name of the administrative subdivision, with all special
#'       characters stripped or replaced with the ASCII equivalents. (character)
#'
#'   The 'change' table includes details about changes to the location hierarchy that
#'   occurred over time: for example, one district splitting into two or the boundary
#'   between two counties shifting.
#'
#'   REQUIRED FIELDS FOR THE 'change' TABLE INCLUDE:
#'   - 'iso': ISO3 code for the country being modeled (character)
#'   - 'year': The four-digit year when the change in the location hierarchy took place.
#'       Changes are applied immediately at the BEGINNING of a year: for example, if a
#'       new administrative unit is evident in a 2007 location snapshot but not in 2006,
#'       the change in this table indicating that new unit's creation should be assigned
#'       to the year 2007. (integer)
#'   - 'change_number': Number uniquely representing a particular change in a given year
#'       (as changes can often span multiple table rows) as well as the order in which it
#'       will be applied to the previous year of data. The order of applying changes
#'       matters only for more complicated multi-step administrative redivisions.
#'       Numbering can be sequential across all years in this table, or numbering can
#'       restart at 1 for each year. (integer)
#'   - 'change_type': The type of change affecting these administrative units. Valid
#'       options include:
#'         * 'SPLIT' - One starting unit splits into two or more units
#'         * 'MERGE' - Two or more starting units merge into a single unit
#'         * 'CODE' - The identifying code for an administrative unit changes, but its
#'             borders remain unaffected
#'         * 'BORDER' - The border between two administrative units changes. In the case
#'             that this border change does not change does not change administrative
#'             codes, 'start_code' and 'end_code' will be the same; for border swaps that
#'             change one or both of the administrative codes, 'start_code' and 'end_code'
#'             can be different.
#'   - 'level': Level of administrative unit that is affected by these changes. Some
#'       administrative redistricting can affect multiple levels of a location hierarchy;
#'       these should be represented by 2+ distinct records with different levels.
#'   - 'start_code': The administrative code ('adm_code' in the snapshot table) for the
#'       administrative unit before the change is applied. For more information, see the
#'       package documentation.
#'   - 'end_code': The administrative code ('adm_code' in the snapshot table) for the
#'       administrative unit after the change is applied. For more information, see the
#'       package documentation.
#'   - 'swap_with': For the 'BORDER' change_type only, what is the uniquely-identifying
#'       code for the administrative unit that swaps territory with this unit? Swaps must
#'       be reciprocal - if one row indicates that unit A swapped with unit B, then
#'       another row with the same 'change_number' must indicate that unit B swapped with
#'       unit A.
#'
#'
#' @param table_name Name of the type of location table being validated. Must be a valid
#'   table name per `get_valid_location_table_names()`.
#' @param input_table The location table to be validated for internal consistency
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
  table_name, input_table, check_years = NULL, raise_issues_as_errors = TRUE
){
  # Check that table and table name are valid
  validate_location_table_name(table_name)
  if(!is.data.table(input_table)) stop(table_name,' pre-validation error: not a data.table')

  # Check that all required fields are present
  field_classes <- get_location_table_field_classes(table_name)
  missing_fields <- setdiff(names(field_classes), names(input_table))
  if(length(missing_fields) > 0){
    stop(
      table_name, " pre-validation error - missing required fields: ",
      paste(missing_fields, collapse=', ')
    )
  }

  # Check that table fields all have the correct classes
  class_mismatch_table <- data.table::data.table(
    field = names(field_classes),
    required_class = field_classes
  )
  class_mismatch_table$class_in_data <- lapply(
    class_mismatch_table$field,
    function(field_name) class(input_table[[field_name]])
  )
  class_mismatch_table <- class_mismatch_table[ required_class != class_in_data , ]
  if(nrow(class_mismatch_table) > 0){
    stop(
      table_name, " pre-validation error: Incorrect classes for required fields:\n",
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
  validation_function <- paste0('validate_location_',table_name,'_table_internal')
  issues_list <- do.call(
    validation_function,
    args = list(
      input_table = input_table,
      check_years = check_years
    )[names(formals(validation_function))]
  )

  # Function output depends on the argument `raise_issues_as_errors`:
  #  - If TRUE, throw an error if there are issues, and pass silently if there are none
  #  - If FALSE, return the list of issues even if it is empty
  if(raise_issues_as_errors==TRUE){
    if(length(issues_list) > 0){
      stop(
        "Validation issues for table ",table_name,":\n  - ",
        paste(issues_list, collapse='\n  - ')
      )
    } else {
      invisible()
    }
  } else {
    return(issues_list)
  }
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
