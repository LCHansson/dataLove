#' Analyse
#' 
#' Analyse data 
#' 
#' @export

analyse <- function(.data) {
  NULL
}



#' @export
dataSummary <- function(.data) {
  NULL
}


#' @export
viewList <- function(.list, recursionLevel, offset = 0) {
  if (missing(recursionLevel)) { recursionLevel <- 0 }
  
  for (i in 1:length(.list)) {
    if (recursionLevel > 0) {
      if (i == length(.list)) { connector <- "+" } else { connector <- "|"}
      padding <- paste0(paste(rep(" ", offset), collapse = ""), connector, "--", collapse = "")
    } else {
      padding <- "--"
    }
    
    name <- names(.list[i])
    if (!is.null(name)) if (name == "") name <- NULL
    if (is.null(name)) name <- paste0("[",i,"]")
    
    cat(paste0(padding, name, "\n"))
    
    if (is.list(.list[[i]])) {
      # Find number of characters with which to offset next level in list
      newOffset <- nchar(padding) + floor(nchar(name) / 2)
      viewList(.list[[i]], recursionLevel = recursionLevel + 1, offset = newOffset)
    }
  }
}


#' @export
findNameColumns <- function(.data, names = NULL) {
  if (is.null(names)) {
    names <- c("namn", "name", "nom")
  }
  
  NULL
}