#' @export
formattedTextList <- function(x) {
  if (class(x) == "factor") {
    x <- as.character(x)
  }
  if (length(x) == 1) {
    return(x)
  }
  if (length(x) == 2) {
    out_string <- paste(x, collapse = " and ")
  } else {
    out_string <- paste0(paste(x[1:(length(x) - 1)], collapse = ", "), ", and ", x[length(x)])
  }
  out_string
}
