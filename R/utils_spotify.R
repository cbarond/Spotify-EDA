#' Converting spaces in a string to '%20
#'
#' @param string A string
#'
#' @return A string.
#' @export
convert_spaces <- function(string) {
  if (!is.null(string) && nchar(string) > 0 && is.character(string)){
    usethis::ui_info("Converting provided string ...")
    converted_string <- string %>% stringr::str_replace_all(" ", "%20")
    return(converted_string)
  } else {
    return(NULL)
  }
}
