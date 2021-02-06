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


#' Get connected subgraphs
#'
#' @description Find connected subgraphs of interacting locations over time
#'
#' @details This is a utility function used to create the 'stable' location table, and is
#'   not exposed to the package user
#'
#' @param edge_matrix [character Matrix] an N-by-2 character matrix, where each 2-item
#'   row lists an edge between two named vertices
#'
#' @return A data.table with two columns: 'vertex' listing each of the named vertices, and
#'   'group' listing unique subgraphs between named vertices (1-indexed)
#'
#' @import data.table igraph
get_connected_subgraphs <- function(edge_matrix){
  # Special case for no entries
  if(nrow(edge_matrix) == 0) return(data.table(vertex=character(0), group=integer(0)))

  # Validate that input dataset is an N-by-2 matrix of character data
  if(!is.matrix(edge_matrix)) stop("Edge matrix must be a matrix")
  if(ncol(edge_matrix) != 2) stop("Edge matrix should have exactly two columns")
  edge_vec <- as.vector(t(edge_matrix))
  if(!'character' %in% class(edge_vec)) stop("Matrix entries must be characters")

  # Convert to graph
  full_graph <- igraph::make_graph(edge_vec, directed=FALSE)

  # Get connected subgraphs and return, with membership
  groupings_vec <- igraph::components(full_graph)$membership
  groupings_dt <- data.table::data.table(
    vertex = names(groupings_vec),
    group = groupings_vec
  )[order(group, vertex)]

  return(groupings_dt)
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
  # Helper function for messaging
  vbmsg <- function(...) if(verbose) message(...)
  vbmsg("Building stable location table:")

  # Validate input arguments
  vbmsg("  - Validating input arguments")
  validate_year_range(year_start = year_start, year_end = year_end)
  year_range <- year_start:year_end
  validate_location_table('change', input_table = change_table, check_years = year_range)
  validate_location_table('annual', input_table = annual_table, check_years = year_range)

  # Merge table creation happens separately for each admin level
  max_level <- max(annual_table$level)
  list_by_level <- vector('list', length=max_level)
  for(thislvl in 1:max_level){
    vbmsg("  - Admin ",thislvl,":")

    # Get subset of change table over this year range
    change_sub <- change_table[(year > year_start) & (year <= year_end) & (level==thislvl),]
    annual_sub <- annual_table[(year >= year_start) & (year <= year_end) & (level==thislvl),]
    # Include only rows that list connections between different admin codes
    change_sub <- change_sub[ start_code != end_code, ]

    # Get connectivity
    subgraphs <- get_connected_subgraphs(
      edge_matrix = as.matrix(change_sub[, .(start_code, end_code)])
    )
    setnames(subgraphs, c('vertex', 'group'), c('adm_code','stable_id'))
    # Exclude any 'dummy' admin codes
    real_adms <- unique(annual_sub$adm_code)
    subgraphs <- subgraphs[adm_code %in% real_adms, ]

    # Create the stable table, which is a combination of unconnected locations and
    #  connected subgraphs
    annual_unconnected <- data.table::data.table(
      adm_code = sort(setdiff(real_adms, unique(subgraphs$adm_code)))
    )
    # The stable_id is indexed at zero
    annual_unconnected[, stable_id := .I - 1 ]
    subgraphs$stable_id <- subgraphs$stable_id + max(annual_unconnected$stable_id)
    stable_this_level <- rbindlist(list(annual_unconnected, subgraphs), use.names=TRUE)
    vbmsg(paste(
      "    -", nrow(stable_this_level), "admin units were grouped into",
      max(stable_this_level$stable_id) + 1, "unique groupings"
    ))

    stable_this_level[, level := thislvl ]
    list_by_level[[thislvl]] <- stable_this_level
  }

  # Combine all levels and add identifying information
  stable_all_levels <- data.table::rbindlist(list_by_level, use.names = TRUE)
  stable_all_levels$iso <- annual_table$iso[1]
  stable_all_levels[, stable_id := as.integer(stable_id) ]
  # Get information about first and last year where an adm_code was found in the dataset
  year_ranges <- annual_table[, .(ymin=min(year), ymax=max(year)), by = .(adm_code,level)]
  stable_all_levels[
    year_ranges,
    `:=` (year_start = ymin, year_end = ymax),
    on = c('adm_code', 'level')
  ]
  data.table::setcolorder(stable_all_levels, get_location_table_required_fields('stable'))

  vbmsg("  - Successfully built stable location table across ",max_level," admin levels.")
  return(stable_all_levels)
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
  year_range <- year_start:year_end
  validate_location_table('annual', input_table = annual_table, check_years = year_range)
  validate_location_table('stable', input_table = stable_table, check_years = year_range)

  stable_year_range <- na.omit(unique(stable_table$year_start, stable_table$year_end))
  if((max(stable_year_range) > year_end) | (min(stable_year_range) < year_start)){
    warning(
      "Stable location table apparently extends beyond the time period ",year_start,'-',
      year_end
    )
  }

  # Merge the stable IDs onto the annual location table
  full_table <- copy(annual_table)
  full_table[stable_table, stable_id := i.stable_id, on = c('adm_code', 'level')]

  # Final column ordering and cleaning
  required_columns <- get_location_table_required_fields('full')
  full_table <- full_table[, ..required_columns]
  data.table::setcolorder(full_table, required_columns)

  return(full_table)
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
