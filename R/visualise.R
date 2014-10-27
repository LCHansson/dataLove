#' Visualise
#' 
#' Visualise data 
#' 
#' @export
visCluster <- function(.data, cluster_column = "cluster") {
  if (!cluster_column %in% names(.data))
    stop("Name of cluster column not in data")
  if (!any(c("factor", "character") %in% class(.data[[cluster_column]])))
    stop("Cluster column is not a factor or a character column")
  
  visualise(.data, layer_points())
}

#' @export
visualise <- function(.data, layer = layer_points()) {
  
  metadata <- findCategories(.data, category_scale_threshold = 0.1)
  
  dimension_cols <- metadata %>%
    filter(probably_category == FALSE & column_class %in% c("numeric", "integer")) %>%
    select(column_name) %>% `[[`(1)
  
  color_cols <- metadata %>%
    filter(probably_category == TRUE) %>%
    select(column_name) %>% `[[`(1)
  
  shinyApp(
    ui = pageWithSidebar(
      headerPanel("Title"),
      sidebarPanel(
        # sliderInput("size", "Area", 10, 1000, value = 500),
        selectInput(
          "xvar", "XVAR",
          choices = dimension_cols,
          selected = dimension_cols[1]
        ),
        selectInput(
          "yvar", "YVAR",
          choices = dimension_cols,
          selected = dimension_cols[2]
        ),
        selectInput(
          "colvar", "Färg",
          choices = color_cols,
          selected = "cluster"
        )
      ),
      mainPanel(
        uiOutput("ggvis_ui"),
        ggvisOutput("ggvis")
      )
    ),
    server = function(input, output) {
      c_data = reactive({
        data <- .data %>%
          select(xvar = matches(input$xvar)[1],
                 yvar = matches(input$yvar)[1],
                 color = matches(input$colvar)[1]
          )
        
        return(data)
      })
      
      c_data %>%
        ggvis(~xvar, ~yvar, fill = ~color) %>%
        layer_points() %>%
        add_tooltip(function(df){
          df$color
        }) %>%
        bind_shiny("ggvis")
    }
  )
}