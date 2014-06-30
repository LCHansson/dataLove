#' cleanData
#' 
#' 
#' 
#' @export

cleanData <- function(.data, .class = NULL, .action = NULL) {
  NULL
}

#' @export
fixUmlauts <- function(.data, src = "åäöüß", target = "ASCII") {
  # Error handling (TODO: Implement support for tabular data)
  if (!is.vector(.data)) stop("Data not a vector!")
  
  dict <- list(
    "å" = "a",
    "ä" = "a",
    "ö" = "o",
    "ü" = "u",
    "ß" = "ss"
  )
  
  if (target == "ASCII") {
    for (char in names(dict)) {
      .data <- str_replace_all(.data, char, dict[[char]])
    }
  }
  
  return(.data)
}
