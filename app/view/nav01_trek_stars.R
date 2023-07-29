# app/view/nav01_trek_stars.R

box::use(
  shiny[...],
  bslib[layout_sidebar, sidebar],
  DT[dataTableOutput],
)

box::use(
  app/logic/colsFilter[...],
)



#' @export
ui <- function(id) {
  ns <- NS(id)
  
  # TODO: How does it look if I put the sidebar in main.R? 
  
  layout_sidebar(
    fillable = TRUE,
    sidebar = sidebar(
      width = 500, 
      title = "nav01 - starrs of trek",
      columnFilterSetUI(ns("filterset"))
    ),
    DT::dataTableOutput(ns("nav01"))
  )
}



#' @export
server <- function(id, cols_id = "default", data) {
  
  print("nav01_trek_stars.R")
  
  moduleServer(id, function(input, output, session) {
    
    print("nav01_trek_stars.R")
    
    selected_data <- data
    
    filtered_data <-
      callModule(
        columnFilterSet,
        id = "filterset",
        cols_id = "default", # TODO - needs changing?  
        dataset = selected_data,
        cols = cols
      )
    
    output$nav01 <- DT::renderDataTable({ filtered_data() })
    
    
  })
}
