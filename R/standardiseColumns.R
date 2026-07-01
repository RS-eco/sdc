#' Standardise Column Names in a Dataframe
#'
#' This internal function standardizes the column names of a given dataframe.
#' It uses regular expressions and to match column names against a list of predefined standard names. 
#' The approach is case-insensitive and allows for flexible matching of column names.
#'
#' @param df A dataframe whose column names need to be standardized.
#' @param verbose A logical indicating whether to print progress messages.
#' @return A dataframe with standardized column names.
#'
#' @keywords internal
standardiseColumns <- function(df, verbose = FALSE) {
  # Internal mapping of standardized names to possible variants
  mapping <- list(
    "x" = "^(?:x|lon|Lon|LON|long|Long|LONG|longitude|Longitude|LONGITUDE)$",
    "y" = "^(?:y|lat|Lat|LAT|latitude|Latitude|LATITUDE)$",
    "sex" = "^(?:sex|gender|females?|m(?:a(?:les?|n)|en)|wom[aey]n)$"
  )
  if (verbose) message("Standardizing column names...")
  
  nms <- names(df)
  for (standard_name in names(mapping)) {
    regex_pattern <- mapping[[standard_name]]
    idx <- which(grepl(regex_pattern, nms))
    if (length(idx) > 0) {
      names(df)[idx[1]] <- standard_name
    }
  }
  return(df)
}