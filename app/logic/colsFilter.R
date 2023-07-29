# app/logic/colsFilter.R

# TODO: All functionality that relies updating input pickers does not work
# for custom input selectors. Example resetting only works for default input
# selector, which means that we are currently not resetting custom pickers.
# This needs to be fixed. A way to fix this is to require the name of the
# update function for the input selector when initiating. "update_with".
# Hmmm... update: I guess it would be best just to hard code some options rather
# than aiming towards a somewhat fully dynamic approach? Need radio_buttons + 
# slider. Multi-input for slider could be converted to slider range rather than
# single input slider. Also need inputPicker with more options. 
# ^ this should be easy enough to integrate use picker_type argument and use 
# if's where picker is used. Remember to do the same in update section. 

# TODO: Should it be possible to set a filter title for each col? I guess it 
# should be possible. Ie. col_name = "gender", title = "Characters gender: "

# TODO: When the reset button is not active the input should be 12 cols wide. 
# I do not think it is like that right now - needs to be tested. 

# ~ Boxed libraries / functions -------------------------------------------

box::use(
  shiny[NS, tags, callModule, 
        isTruthy, req,
        renderUI, uiOutput,
        reactive, reactiveVal, reactiveValues, freezeReactiveValue, 
        observeEvent,
        actionButton],
  
  dplyr[filter, select],
  
  bslib[page_fillable, layout_columns], 
  
  bsicons[bs_icon], 
  
  shinyWidgets[pickerInput, updatePickerInput],
)

box::use(
  app/logic/colsGlobalVault[colsGlobalVault],
)



# ~ Function and module exports -------------------------------------------

#' @export
columnFilterUI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("filter_container"))
}



#' @export
columnFilter <- function(input, output, session, df, reset_btn_id, reset_btn, choice_filter, col_num, col_name, picker_type, picker_code, pick_multiple, pick_selected) {
  
  # Set multiple argument default as TRUE, ie. when pick_multiple has a length
  # of 0. this makes it possible to omit setting arguments when initiating
  # module. This only accounts for non-custom input pickers. 
  if (length(pick_multiple) == 0) {
    pick_multiple <- TRUE
  }
  
  filter_values <- lapply(1:col_num, function(i) reactiveValues(choices = NULL, current_values = NULL))
  
  # Frame changes. (i.e. it doesn't re-render when filters change state.)
  output$filter_container <- renderUI({
    # Don't render if col_num is > actual number of cols
    req(col_num <= ncol(df()))

    # TRUE == default picker, FALSE == custom picker
    if (length(picker_type) == 0) { # Same as above - length = 0 == default
      
      freezeReactiveValue(input, "filter_value")
        pickerInput(session$ns("filter_value"),
                    label = col_name,
                    choices = sort(unique(df()[, col_num, drop = TRUE])),
                    multiple = pick_multiple,
                    selected = pick_selected  
      )
    } else { # Input for setting up custom pickers 
      freezeReactiveValue(input, "filter_value")
      eval(parse(text = picker_code)) 
    } # ^-- With parse and eval we expect definition of a custom picker to be 
  })  #     text and not a function. Maybe change this? 
  
  # When the other filters change, update this filter to remove rows that
  # are filtered out by the other filters' criteria. (We also add in the
  # currently selected values for this filter, so that changing other
  # filters does not cause this filter's selected values to be unselected;
  # while that behavior might make sense logically, it's a poor user
  # experience.)
  observeEvent(choice_filter(), {
    current_values <- input$filter_value
    
    filter_values[[col_num]]$current_values <- current_values

    updatePickerInput(session, "filter_value",
                      choices = sort(unique(c(current_values, df()[choice_filter(),col_num,drop=TRUE]))),
                      selected = current_values
    )
    
    filter_values[[col_num]]$choices <- sort(unique(c(current_values, df()[choice_filter(), col_num, drop = TRUE])))
  })
  
  # When a reset_btn is pressed, reset its values to null before updating.
  # The values are then set to their initial state when the app was first loaded
  # to maintain consistency.
  observeEvent(reset_btn(), {
    # Hard "reset" current values 
    filter_values$current_values <- NULL 
    
    # Update the input selector to contain initial choices/selection from when 
    # when the app was first loaded.
    updatePickerInput(session, "filter_value",
                      choices = sort(unique(df()[, col_num, drop = TRUE])),
                      selected = pick_selected)
  })
  
  # Return a reactive that is a row index of selected rows, according to
  # just this filter. If this filter shouldn't be taken into account
  # because its col_num is too high, or if there are no values selected,
  # just return TRUE to accept all rows.
  reactive({
    if (col_num > ncol(df())) {
      TRUE
    } else if (!isTruthy(input$filter_value)) {
      TRUE
    } else {
      df()[, col_num, drop = TRUE] %in% input$filter_value
    }
  })
  }

#' @export
columnFilterSetUI <- function(id, cols_id = "default") {
  ns <- NS(id)
  
  page_fillable(
    lapply(1:colsGlobalVault$getColMax(cols_id), function(i) {
      col <- colsGlobalVault$getColsPrivate(cols_id)[[i]]
      if (col$filter_active) {
        if (col$reset_button) {
          reset_button <- actionButton(
            ns(paste0("btn_reset", i)),
            label = bs_icon("arrow-counterclockwise"),
            class = "colsfilter-reset-btn",
            style = "margin-top: 32px;" # A not so elegant fix for vertical misalignment with selector. 
          )
        } else {
          # Invisible placeholder - fix for graphical misalignment when there ís no reset button. 
          reset_button <- tags$div(style = "width: 40px; opacity: 0;") 
        }
        layout_columns(
          class = "colsfilter-picker-reset-row",
          fillable = FALSE,
          fill = TRUE,
          col_widths = c(11, 1),
          
          columnFilterUI(ns(paste0("col", i))), # filters
          reset_button
        )
          
      } else {
        # Filter not set to  active - so, let's render nothing.
      }
    }),
    if (colsGlobalVault$getClearAllState(cols_id)) {
      actionButton(ns("clear_all_button"), 
                   "Reset everything",
                   style = "margin-top: 28px") # Add some more space between last selector and button.
    }
  )
}


#' @export
columnFilterSet <- function(input, output, session, cols_id = "default", dataset, cols, pick_multiple, maxcol) {
  
  # Reorder columns so that the selected ones for filtering are first. Just
  # before data has been filtered and send back to the caller of the module, it
  # will be reordered to the original order to retain consistency.
  cols <- colsGlobalVault$getColsPrivate(cols_id)
  col_names <- colsGlobalVault$getColNames(cols_id)
  df <- reactive({ dataset()[, c(col_names, setdiff(names(dataset()), col_names))] })
  
  maxcol <- colsGlobalVault$getColMax(cols_id)
  
  # Each column filter needs to only display the choices that are
  # permitted after all the OTHER filters have had their say. But
  # each column filter must not take its own filter into account
  # (hence we do filter[-col], not filter, in the reactive below).
  create_choice_filter <- function(col) {
    reactive({
      filter_values <- lapply(filters[-col], do.call, args = list())
      Reduce(`&`, filter_values, TRUE)
    })
  }
  
  reset_btns <- lapply(1:maxcol, function(i) {
    reset_btn_id <- paste0("btn_reset", i)
    reset_btn <- reactiveVal() # Initialize as empty reactive value to avoid triggering it at app initiation
    observeEvent(input[[reset_btn_id]], {
      # Set the reset button to 1 when observeEvent is triggered for the first time, triggering the first reset
      if (is.null(reset_btn())) {
        reset_btn(1)
      } else {
        reset_btn(reset_btn() + 1)  # ... triggering reactivity the 2nd, 3rd etc. reset.
      }
    })
    reset_btn
  })
  
  # Filters is a list of reactive expressions, each of which is a
  # logical vector of rows to be selected.
  filters <- lapply(1:maxcol, function(i) {
    
    reset_btn_id <- paste0("btn_reset", i)
    reset_btn <- reset_btns[[i]]
    
    callModule(
      columnFilter,
      id = paste0("col", i),
      df = df,
      col_num = i,
      col_name = cols[[i]]$col_name,           
      picker_type = cols[[i]]$picker_type,     
      picker_code = cols[[i]]$picker_code,     
      pick_multiple = cols[[i]]$pick_multiple, 
      pick_selected = cols[[i]]$pick_selected, 
      choice_filter = create_choice_filter(i),
      reset_btn = reset_btn 
    )
  })
  
  observeEvent(input$clear_all_button, {
    lapply(reset_btns, function(reset_btn) {
      reset_btn(reset_btn() + 1) # Force reactivity
    })
  })
  
  # Export data to caller
  reactive({
    filter_values <- lapply(filters, do.call, args = list())
    selected_rows <- Reduce(`&`, filter_values, TRUE)
    
    col_names_all <- names(dataset())
    
    # Using dplyr rather than R-base for reordering columns back to their
    # original order and filtering data. dplyr is much faster.
    df() |> 
      filter(selected_rows) |> 
      select(all_of(col_names_all))
  })
  
}
