#' Title
#'
#' @return
#' @export
#'
#' @examples
config_temp = function () {
  if (do::is.windows()) {
    temp <- paste0(do::upper.dir(do::upper.dir(list.files(.libPaths(),
                                                          pattern = "easyFAERS", full.names = TRUE))), "easyFAERS")
    temp <- do::last(temp[which.max(nchar(temp))])
    if (!dir.exists(temp))
      dir.create(temp, showWarnings = FALSE, recursive = TRUE)
  }
  else {
    temp <- paste0(do::upper.dir(do::upper.dir(do::upper.dir(.libPaths()))),
                   "easyFAERS")
    temp <- do::last(temp[which.max(nchar(temp))])
    if (!dir.exists(temp))
      dir.create(temp, showWarnings = FALSE, recursive = TRUE)
  }
  return(temp)
}
