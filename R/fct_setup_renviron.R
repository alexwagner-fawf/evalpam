#' setup_renviron
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
setup_renviron <- function(filepath = here::here(".Renviron"),
                           overwrite = TRUE,
                           evalpam_pw = "4zB|cn{u&IvWF?YEy~0b%({-^Ct",
                           spectogram_folder = NA) {

  if(file.exists(filepath) && !overwrite){
    message(".Renviron exists. To replace, set overwrite = TRUE")
    return(invisible(NULL))
  }

  # Set default spectogram folder if NA
  if(is.na(spectogram_folder)){
    spectogram_folder <- file.path(dirname(filepath), "spectograms")
  }

  if(!dir.exists(spectogram_folder)) dir.create(spectogram_folder)

  # Encode password
  evalpam_pw_encoded <- base64enc::base64encode(charToRaw(evalpam_pw))

  # Prepare lines
  lines <- c(
    paste0("evalpam_pw=", evalpam_pw_encoded),
    paste0("spectogram_folder=", spectogram_folder)
  )

  # Write each line to .Renviron
  writeLines(lines, con = filepath)

  # Load the .Renviron
  readRenviron(filepath)

  message(".Renviron written to ", filepath)
  invisible(TRUE)
}
