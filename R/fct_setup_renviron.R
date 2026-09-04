#' setup_renviron
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
setup_renviron <- function(filepath = here::here(".Renviron"),
                           overwrite = TRUE,
                           spectrogram_folder = NA) {

  if(file.exists(filepath) && !overwrite){
    message(".Renviron exists. To replace, set overwrite = TRUE")
    return(invisible(NULL))
  }

  if(is.na(spectrogram_folder)){
    spectrogram_folder <- file.path(dirname(filepath), "spectrograms")
  }

  if(!dir.exists(spectrogram_folder)) dir.create(spectrogram_folder)

  lines <- paste0("spectrogram_folder=", spectrogram_folder)

  writeLines(lines, con = filepath)
  readRenviron(filepath)

  message(".Renviron written to ", filepath)
  invisible(TRUE)
}
