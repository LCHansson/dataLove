#' Analyse
#' 
#' Analyse data 
#' 
#' @export

analyse <- function (.data) {
  NULL
}



#' @export
dataSummary <- function (.data) {
  NULL
}


#' @export
findNameColumns <- function (.data, name_strings = NULL, regex = FALSE, return_vector = FALSE) {
  if (is.null(name_strings)) {
    name_strings <- c("namn", "name", "nom", "id")
  }
  
  nms <- tolower(names(.data))
  
  are_probably_names <- nms %>% sapply(function (x) {
    if (regex) {
      x %>% str_detect(name_strings) %>% any
    } else {
      return(x %in% name_strings)
    }
  })
  
  if (return_vector)
    return(are_probably_names)
  else
    return(nms[are_probably_names])
}


#' @export
findCategories <- function (.data, category_scale_threshold = 0.20, assume_numerics_not_category = TRUE) {
  classes <- sapply(.data, function (x) {
    cl <- class(x)
    if ("ordered" %in% cl)
      cl <- cl[!cl %in% "ordered"]
    
    cl
  })
  
  metadata <- lapply(.data, function (col) {
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
    select(column_name, column_class, unique_to_length_ratio, probably_category) %>%
    mutate(probably_name = findNameColumns(.data, return_vector = TRUE))
  
  metadata
}

#' @export
findOutliers <- function (
  .data,
  number_of_observations = 5,
  connections = 1,
  outlier_sd_threshold = 3
) {
  metadata <- findCategories(.data, category_scale_threshold = 0.1)
  
  cols <- metadata %>% filter(probably_category == FALSE & column_class %in% c("numeric", "integer")) %>%
    select(column_name) %>% `[[`(1)
  
  scoredData <- .data[,cols] %>% scores(type = "z") %>% tbl_df
  
  lower <- sapply(names(scoredData), function (nm) {
    #     data.frame(
    data = head(.data[order(scoredData[[nm]]),nm] , n = 5L)
    #       outlier_rank = head(scoredData[order(scoredData[[nm]]),nm], n = 5L)
    #     )
  }) %>%
    data.frame %>% tbl_df

  upper <- sapply(names(scoredData), function (nm) {
    #     data.frame(
    data = head(.data[order(scoredData[[nm]], decreasing = TRUE),nm] , n = 5L)
    #       outlier_rank = head(scoredData[order(scoredData[[nm]], decreasing = TRUE),nm], n = 5L)
    #     )
  }) %>%
    data.frame %>% tbl_df
  
}


#' @export
findClusters <- function (.data, method = c("kmeans"), k = 5) {
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
