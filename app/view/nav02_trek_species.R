# app/view/nav01_trek_stars.R

box::use(
  dplyr[...],
  shiny[...],
  bslib[...],
)

# box::use(
#   app/logic/st_transformations[st_grp_count],
# )



#' @export
ui <- function(id) {
  ns <- NS(id)
  
  # TODO: How does it look if I put the sidebar in main.R? 
  
  layout_sidebar(
    fillable = TRUE,
    sidebar = sidebar(
      title = "nav01 - species of trek",
      varSelectInput(
        "color_by", "Color by",
        c("species", "island", "sex"),
        selected = "species"
      )
    ),
    
    layout_column_wrap(
      width = 1/2,
      fill = FALSE,
      
      "___--'''- smuk chart",
      
      value_box(
        "Average bill",
        uiOutput("2"),
        showcase = bsicons::bs_icon("currency-dollar"),
        theme_color = "success"
      )
    )
  )
}



#' @export
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
  })
}
