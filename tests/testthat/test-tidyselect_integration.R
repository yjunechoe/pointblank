tbl <- data.frame(x = 1:2, y = 1:2, nonunique = "A")
exist_col <- "y"
nonunique_col <- "nonunique"
nonexist_col <- "z"

test_that("Backwards compatibility with `vars()`", {
  
  # Bare symbol selects column(s)
  expect_success(expect_rows_distinct(tbl, vars(x)))
  expect_success(expect_rows_distinct(tbl, vars(x, nonunique)))
  expect_failure(expect_rows_distinct(tbl, vars(nonunique)))
  
  # Bare character selects column(s)
  expect_success(expect_rows_distinct(tbl, vars("x")))
  expect_success(expect_rows_distinct(tbl, vars("x", "nonunique")))
  expect_failure(expect_rows_distinct(tbl, vars("nonunique")))
  
  # Bang-bang in-lines value
  expect_success(expect_rows_distinct(tbl, vars(!!exist_col)))
  expect_failure(expect_rows_distinct(tbl, vars(!!nonunique_col)))
  
  # `vars()` wrapping tidyselect expressions is redundant but continues to work
  expect_success(expect_rows_distinct(tbl, vars(all_of("x"))))
  
  # `vars()` selection of 0-columns errors *only* in non-validation-planning contexts
  expect_error(rows_distinct(tbl, vars("z")))
  expect_error(expect_rows_distinct(tbl, vars("z")))
  expect_error(test_rows_distinct(tbl, vars("z")))
  expect_no_error(tbl %>% create_agent() %>% rows_distinct(vars("z")))
  expect_no_error(tbl %>% create_agent() %>% rows_distinct(vars("z")) %>% interrogate())
  
})

test_that("Full range of tidyselect features available in column selection", {
  
  # Single symbol
  expect_success(expect_rows_distinct(tbl, x))
  expect_failure(expect_rows_distinct(tbl, nonunique))
  
  # Preferred {tidyselect}-style `c()` syntax
  expect_success(expect_rows_distinct(tbl, c(x)))
  expect_success(expect_rows_distinct(tbl, c(x, nonunique)))
  expect_failure(expect_rows_distinct(tbl, c(nonunique)))
  
  # {tidyselect} functions
  expect_success(expect_rows_distinct(tbl, tidyselect::all_of("x")))
  expect_success(expect_rows_distinct(tbl, tidyselect::all_of(c("x", "nonunique"))))
  expect_failure(expect_rows_distinct(tbl, tidyselect::all_of("nonunique")))
  
  # NEW: {tidyselect} integer indexing
  expect_success(expect_rows_distinct(tbl, 1))
  expect_success(expect_rows_distinct(tbl, c(1, 3)))
  expect_failure(expect_rows_distinct(tbl, 3))
  
  # NEW: {tidyselect} negative indexing
  expect_success(expect_rows_distinct(tbl, -(2:3)))
  expect_success(expect_rows_distinct(tbl, -2))
  expect_failure(expect_rows_distinct(tbl, -(1:2)))
  
  # NEW: {tidyselect} `where()` predicate:
  expect_success(expect_rows_distinct(tbl, !tidyselect::where(is.character)))
  expect_success(expect_rows_distinct(tbl, tidyselect::where(is.numeric)))
  expect_failure(expect_rows_distinct(tbl, tidyselect::where(is.character)))
  
  # NEW: {tidyselect} functions in complex expressions
  expect_success(expect_rows_distinct(tbl, c(x, tidyselect::all_of(exist_col))))
  expect_error(expect_rows_distinct(tbl, c(x, tidyselect::all_of(nonexist_col))))
  expect_success(expect_rows_distinct(tbl, c(x, tidyselect::any_of(nonexist_col))))
  
  # Supplying a character vector variable still works, but signals deprecation:
  rlang::local_options(lifecycle_verbosity = "warning")
  expect_success(expect_warning(
    expect_rows_distinct(tbl, exist_col),
    "Using an external vector in selections was deprecated in tidyselect 1.1.0."
  ))
  
})

test_that("'NULL = select everything' behavior in rows_*() validation functions", {

  # For `rows_*()` functions specifically, empty/NULL = "select everything" behavior:
  expect_success(expect_rows_distinct(data.frame(x = 1, y = 2)))
  expect_success(expect_rows_complete(data.frame(x = 1, y = 2)))
  expect_failure(expect_rows_distinct(data.frame(x = c(1, 1))))
  expect_failure(expect_rows_complete(data.frame(x = c(1, NA))))
  expect_success(expect_rows_distinct(data.frame(x = 1, y = 2), columns = NULL))
  expect_success(expect_rows_complete(data.frame(x = 1, y = 2), columns = NULL))
  expect_failure(expect_rows_distinct(data.frame(x = c(1, 1)), columns = NULL))
  expect_failure(expect_rows_complete(data.frame(x = c(1, NA)), columns = NULL))
  
  # Report shows all column names with empty `columns` argument
  expect_equal({
    small_table %>%
      create_agent() %>% 
      rows_distinct() %>% 
      rows_complete() %>% 
      interrogate() %>% 
      {.$validation_set$column} %>% 
      unlist() %>% 
      unique()
  }, toString(colnames(small_table)))
  
  # Report shows all column names with explicit NULL `columns` argument
  expect_equal({
    small_table %>%
      create_agent() %>% 
      rows_distinct(columns = NULL) %>% 
      rows_complete(columns = NULL) %>% 
      interrogate() %>% 
      {.$validation_set$column} %>% 
      unlist() %>% 
      unique()
  }, toString(colnames(small_table)))
    
})

# tidyselect coverage for `col_exists()`
test_that("'NULL = select everything' behavior in rows_*() validation functions", {
  
  # Reprex from (#433)
  df <- tibble::tibble(
    id.x = 1:3,
    id.y = 1:3,
    stuff = 1:3
  )
  expect_success({
    df %>% 
      expect_col_exists(
        columns = vars(ends_with(".x"))
      )
  })
  expect_equal({
    df %>% 
      col_exists(
        columns = vars(ends_with(".x"))
      )
  }, df)
  
  # Multiple column selection produces multiple steps
  expect_no_error({
    df_interrogated <- df %>% 
      create_agent() %>% 
      col_exists(starts_with("id")) %>% 
      interrogate()
  })
  expect_equal(nrow(df_interrogated$validation_set), 2L)
  
})

test_that("`info_columns()` get `where()` support for simple `is.*()`-style class predicates", {
  
  # Reprex from examples selecting `date` and `date_time` columns
  informant <- 
    create_informant(
      tbl = ~ small_table,
      tbl_name = "small_table",
      label = "An example."
    )
  # tidyselect by column name pattern
  informant_1 <- informant %>% 
    info_columns(
      columns = starts_with("date"),
      info = "Time-based values (e.g., `Sys.time()`)."
    )
  
  # NEW: tidyselect with class predicates using `where()`
  ## `is_timepoint()` is a copy of `lubridate::is.timepoint()`
  is_timepoint <- function(x) {
    inherits(x, c("POSIXt", "POSIXct", "POSIXlt", "Date"))
  }
  informant_2 <- informant %>% 
    info_columns(
      columns = where(is_timepoint),
      info = "Time-based values (e.g., `Sys.time()`)."
    )
  expect_identical(informant_1, informant_2)
  
  # NEW: complex tidyselect expressions
  informant_3 <- informant %>% 
    info_columns(
      columns = where(is.character) & matches("[abc]"),
      info = "Time-based values (e.g., `Sys.time()`)."
    )
  # Columns `b` and `f` are marked as character in the columns metadata
  expect_equal(
    names(which(sapply(informant_3$metadata$columns, `[[`, "_type") == "character")),
    c("b", "f")
  )
  # Complex expression `where(is.character) & matches("[abc]")` adds info to `b` but not `f`
  expect_equal(
    sapply(informant_3$metadata$columns[c("b", "f")], `[[`, "info"),
    list(b = "Time-based values (e.g., `Sys.time()`).", f = NULL)
  )
  
  ## NOTE: `where()` predicates only work for function checking type/class
  ## - Ex: something like `where(\(x) max(x) > 100)` doesn't work because
  ##       `info_columns()` doesn't have access to the full data
  
})
