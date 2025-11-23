#' setup_renviron
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
setup_renviron <- function(filepath = here::here(".Renviron"),
                           overwrite = TRUE,
                           evalpam_pw = "4zB|cn{u&IvWF?YEy~0b%({-^Ct"){
  if(file.exists(filepath) && !overwrite){
    message(".Renviron exists. To replace, set overwrite = TRUE")
  }else{
    paste(
      paste0("evalpam_pw=",base64enc::base64encode(charToRaw(evalpam_pw))),
      collapse = "\n"
    ) |>
      write(filepath)
  }
}
