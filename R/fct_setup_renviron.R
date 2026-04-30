#' setup_renviron
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
setup_renviron <- function(filepath = here::here(".Renviron"),
                           overwrite = TRUE,
                           spectogram_folder = NA) {

  if(file.exists(filepath) && !overwrite){
    message(".Renviron exists. To replace, set overwrite = TRUE")
    return(invisible(NULL))
  }

  if(is.na(spectogram_folder)){
    spectogram_folder <- file.path(dirname(filepath), "spectograms")
  }

  if(!dir.exists(spectogram_folder)) dir.create(spectogram_folder)

  lines <- paste0("spectogram_folder=", spectogram_folder)

  writeLines(lines, con = filepath)
  readRenviron(filepath)

  message(".Renviron written to ", filepath)
  invisible(TRUE)
}
