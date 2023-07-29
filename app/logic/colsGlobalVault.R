# app/logic/colsFilter.R

# Intention: This R6 class object provcols_ides explicit setter and getter functions
# to create a clear and controlled interface for accessing and modifying data
# across environments. It ensures data integrity and enhances safety by
# maintaining a controlled access mechanism.

box::use(
  R6[...],
)

#' @export
colsGlobalVault <- R6Class(
  "colsGlobalVault",
  private = list(
    datasets = list() # private storage for (more than one) dataset
  ),
  
  public = list(
    getColsPrivate = function(cols_id = "default") {
      private$datasets[[cols_id]]$colsPrivate
    },
    setColsPrivate = function(cols_id = "default", data) {
      if (!is.null(private$datasets[[cols_id]]$colsPrivate)) {
        message(paste("Warning: Overwriting existing private data for dataset", cols_id))
      }
      data <- lapply(data, function(col) {
        if (!is.null(col$col_name)) {
          col$filter_active <- TRUE
        }
        col
      })
      data <- c(data, list(list(clear_all_button = TRUE)))
      private$datasets[[cols_id]]$colsPrivate <- data
    },
    getColNames = function(cols_id = "default") {
      column_cfg <- self$getColsPrivate(cols_id)
      col_names <- sapply(column_cfg, function(col) col$col_name)
      flat_col_names <- unlist(col_names)
      flat_col_names
    },
    getColMax = function(cols_id = "default") {
      length(self$getColNames(cols_id))
    },
    getClearAllState = function(cols_id = "default") {
      column_cfg <- self$getColsPrivate(cols_id)
      btn_state <- sapply(column_cfg, function(col) col$clear_all_button)
      unlist(btn_state)
    },
    setClearAllState = function(cols_id = "default", state) {
      colsPrivate <- self$getColsPrivate(cols_id)
      col_index <- match(TRUE, sapply(colsPrivate, function(col) col$clear_all_button))
      private$datasets[[cols_id]]$colsPrivate[[col_index]]$clear_all_button <- state
    },
    setFilterActive = function(cols_id = "default", col_name, active) {
      colsPrivate <- self$getColsPrivate(cols_id)
      col_index <- match(col_name, sapply(colsPrivate, function(col) col$col_name))
      if (!is.na(col_index)) {
        private$datasets[[cols_id]]$colsPrivate[[col_index]]$filter_active <- active
      } else {
        stop("Error: Column name not found - ", col_name)
      }
    },
    isFilterActive = function(cols_id = "default", col_name) {
      column_cfg <- self$getColsPrivate(cols_id)
      col <- column_cfg[[match(col_name, sapply(column_cfg, function(col) col$col_name))]]
      if (!is.null(col)) {
        col$filter_active
      } else {
        stop("Error: Column name not found - ", col_name)
      }
    }
  ),
  lock_class = TRUE # We do not want any more members added.
)$new()

#' @export
setColsGlobal <- function(cols_id = "default", cols) {
  
  colsGlobalVault$setColsPrivate(cols_id, cols)
}
