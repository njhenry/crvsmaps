## -------------------------------------------------------------------------------------->
##
## Functions for creating tables used to define location sets and hierarchies for spatial
## CRVS analysis. These tables include:
##  - Location hierarchy snapshots (blank templates, to be filled by user)
##  - Location change histories (blank templates, to be filled by user)
##  - Location hierarchies over a time period
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
#' @details To create a valid location hierarchy over a period of time, the user needs to
#'   create two tables representing that location hierarchy, one representing snapshots of
#'   the location hierarchy in particular years, and the other representing changes to the
#'   location hierarchy across years. These tables are validated against each other and
#'   then used to construct useful data structures for CRVS analysis and modeling. For
#'   more details and examples of these tables, check the documntation: `help(mapcrvs)`
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
#'       should be input as <NA> strings. (character)
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
#' @param table_name [char] Name of a valid location table. Valid options include
#'   'snapshot' and 'change'
#'
#' @return A data.table with a single row of <NA> data entries, formatted with the correct
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
