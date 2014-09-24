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

#' @export
findCategories <- function(.data, category_scale_threshold = 0.20, assume_numerics_not_category = TRUE) {
  classes <- sapply(.data, function(x) {
    cl <- class(x)
    if ("ordered" %in% cl)
      cl <- cl[!cl %in% "ordered"]
    
    cl
  })
  
  metadata <- lapply(.data, function(col) {
    column_class = class(col)
    if ("ordered" %in% column_class)
      column_class <- column_class[!column_class %in% "ordered"]
    unique_to_length_ratio = length(unique(col)) / length(col)
    probably_category = ifelse(
      column_class %in% c("character", "factor") | !assume_numerics_not_category,
      unique_to_length_ratio <= category_scale_threshold,
      FALSE
    )
    
    c(column_class = column_class, unique_to_length_ratio = unique_to_length_ratio, probably_category = probably_category)
  }) %>%
    data.frame %>%
    t %>%
    data.frame %>%
    tbl_df
  metadata <- metadata %>%
    mutate(column_name = row.names(metadata)) %>%
    select(column_name, column_class, unique_to_length_ratio, probably_category)
  
  metadata
}

#' @export
findOutliers <- function(
  .data,
  number_of_observations = 5,
  connections = 1,
  outlier_sd_threshold = 3
) {
  metadata <- findCategories(.data, category_scale_threshold = 0.1)
  
  cols <- metadata %>% filter(probably_category == FALSE & column_class %in% c("numeric", "integer")) %>%
    select(column_name) %>% `[[`(1)
  
  scoredData <- .data[,cols] %>% scores(type = "z") %>% tbl_df
  
  lower <- sapply(names(scoredData), function(nm) {
    #     data.frame(
    data = head(.data[order(scoredData[[nm]]),nm] , n = 5L)
    #       outlier_rank = head(scoredData[order(scoredData[[nm]]),nm], n = 5L)
    #     )
  }) %>%
    data.frame %>% tbl_df

  upper <- sapply(names(scoredData), function(nm) {
    #     data.frame(
    data = head(.data[order(scoredData[[nm]], decreasing = TRUE),nm] , n = 5L)
    #       outlier_rank = head(scoredData[order(scoredData[[nm]], decreasing = TRUE),nm], n = 5L)
    #     )
  }) %>%
    data.frame %>% tbl_df
  
}


#' @export
findClusters <- function(.data, method = c("kmeans"), k = 5) {
  if (any(colSums(is.na(.data)) > 0))
    warning("Cannot perform clustering on columns with NA, so dropping any columns with NA in them.")
  .data <- .data[,colSums(is.na(.data)) == 0]

  metadata <- findCategories(.data, category_scale_threshold = 0.1)
  
  cols <- metadata %>% filter(probably_category == FALSE & column_class %in% c("numeric", "integer")) %>%
    select(column_name) %>% `[[`(1)
  
  cluster_data <- .data[,cols]
  centered_data <- kmeans(cluster_data, centers = k)
  
  .data <- .data %>%
    mutate(cluster = factor(centered_data$cluster)) %>%
    tbl_df
  
  .data
}
