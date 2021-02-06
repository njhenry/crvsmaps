## -------------------------------------------------------------------------------------->
##
## Low-level utility functions for the mapcrvs package - not exposed to package users
##
## -------------------------------------------------------------------------------------->

#' Message data.frame
#'
#' @description Print a data.frame to screen using the `message()` function
#'
#' @param df Data.frame to print out
#'
#' @return (No return) prints data.table to stderr stream
#'
message_data_frame <- function(df){
  message(paste0(capture.output(df), collapse='\n'))
  invisible()
}
