# app/main.R

# ~ boxed libraries / functions -------------------------------------------

box::use(
  shiny[...],
  bslib[...],
  readr[read_rds],
)

# TODO: picker_type argument - bruges det stadig? nej vel?
box::use(
  app/logic/colsGlobalVault[...],
)



# ~ import boxed views ----------------------------------------------------

box::use(
  app/view/nav01_trek_stars,
  app/view/nav02_trek_species,
  # app/view/nav03
)



# ~ global data -----------------------------------------------------------

# TODO: fix uid.x / uid.y 
global_df_st_all <- reactive({
  read_rds("./build_trek_data/data/df_stapi_flat.rds")
})



# ~ global filter columns -------------------------------------------------

cols_st_all <- list(
  list(col_name = "name", reset_button = TRUE),
  list(col_name = "movies.title", reset_button = FALSE),
  list(col_name = "episodes.title", reset_button = TRUE),
  list(col_name = "height", reset_button = TRUE)
)

setColsGlobal(cols_id = "default", cols = cols_st_all)



# ~ ui --------------------------------------------------------------------

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  page_navbar(
    theme = bs_theme(
      primary = "#63130A", 
      font_scale = NULL, 
      bootswatch = "simplex"
    ),
    title = "Navbar title",
    nav_panel(title = "nav01: Trek Stars", nav01_trek_stars$ui(ns("nav01"))),
    nav_panel(title = "nav02: Trek species", "hej :)")
  )
}



# ~ server ----------------------------------------------------------------

#' @export
server <- function(id, input, output, session, data, df) {
  
  bslib::bs_themer()

  moduleServer(id, function(input, output, session, data, df) {
    
    nav01_trek_stars$server(id = "nav01",
                            cols_id = "default",
                            data = global_df_st_all)
  })
}
