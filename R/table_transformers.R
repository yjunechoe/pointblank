#
#                _         _    _      _                _    
#               (_)       | |  | |    | |              | |   
#  _ __    ___   _  _ __  | |_ | |__  | |  __ _  _ __  | | __
# | '_ \  / _ \ | || '_ \ | __|| '_ \ | | / _` || '_ \ | |/ /
# | |_) || (_) || || | | || |_ | |_) || || (_| || | | ||   < 
# | .__/  \___/ |_||_| |_| \__||_.__/ |_| \__,_||_| |_||_|\_\
# | |                                                        
# |_|                                                        
# 
# This file is part of the 'rich-iannone/pointblank' package.
# 
# (c) Richard Iannone <riannone@me.com>
# 
# For full copyright and license information, please look at
# https://rich-iannone.github.io/pointblank/LICENSE.html
#


#' Table Transformer: obtain a summary stats table for numeric columns
#' 
#' @description
#' With any table object, you can produce a summary table that is scoped to
#' the numeric column values. The table produced will have a leading column
#' called `".stat."` with labels for each of the nine rows, each corresponding
#' to the following summary statistics:
#' 
#' 1. Minimum (`"min"`)
#' 2. 5th Percentile (`"p05"`)
#' 3. 1st Quartile (`"q_1"`)
#' 4. Median (`"med"`)
#' 5. 3rd Quartile (`"q_3"`)
#' 6. 95th Percentile (`"p95"`)
#' 7. Maximum (`"max"`)
#' 8. Interquartile Range (`"iqr"`)
#' 9. Range (`"range"`)
#' 
#' Only numerical data from the input table will generate columns in the output
#' table. Column names from the input will be used in the output, preserving
#' order as well.
#' 
#' @param tbl A table object to be used as input for the transformation. This
#'   can be a data frame, a tibble, a `tbl_dbi` object, or a `tbl_spark` object.
#' 
#' @return A `tibble` object.
#' 
#' @examples 
#' # Get summary statistics for the
#' # `game_revenue` dataset that's
#' # included in the package
#' tt_summary_stats(game_revenue)
#' 
#' # Ensure that the maximum revenue
#' # for individual purchases in the
#' # `game_revenue` table is less than
#' # $150
#' tt_summary_stats(game_revenue) %>%
#'   col_vals_lt(
#'     columns = vars(item_revenue),
#'     value = 150,
#'     segments = .stat. ~ "max"
#'   )
#' 
#' # For in-app purchases in the
#' # `game_revenue` table, check that
#' # median revenue is somewhere
#' # between $8 and $12
#' game_revenue %>% 
#'   dplyr::filter(item_type == "iap") %>%
#'   tt_summary_stats() %>%
#'   col_vals_between(
#'     columns = vars(item_revenue),
#'     left = 8, right = 12,
#'     segments = .stat. ~ "med"
#'   )
#' 
#' @family Table Transformers
#' @section Function ID:
#' 12-1
#' 
#' @export
tt_summary_stats <- function(tbl) {
  
  # Determine whether the `tbl` object is acceptable here
  check_is_a_table_object(tbl = tbl)
  
  n_cols <- get_table_total_columns(data = tbl)
  
  tbl_info <- get_tbl_information(tbl = tbl)
  col_names <- tbl_info$col_names
  r_col_types <- tbl_info$r_col_types
  
  summary_stats_tbl <- 
    dplyr::tibble(
      `.stat.` = c(
        "min", "p05", "q_1", "med", "q_3",
        "p95", "max", "iqr", "range"
      )
    )
  
  for (i in seq_len(n_cols)) {
    
    if (r_col_types[i] %in% c("integer", "numeric")) {
      
      data_col <- dplyr::select(tbl, col_names[i])
      
      # nocov start
      
      suppressWarnings({
        if (inherits(tbl, "data.frame")) {
          stats_list <- get_df_column_qtile_stats(data_column = data_col)
        } else if (inherits(tbl, "tbl_dbi")) {
          stats_list <- get_dbi_column_qtile_stats(data_column = data_col)
        } else if (inherits(tbl, "tbl_spark")) {
          stats_list <- get_spark_column_qtile_stats(data_column = data_col)
        }
      })
      
      # nocov end
      
      stats_col <- 
        tibble::enframe(
          unlist(stats_list),
          name = NULL,
          value = col_names[i]
        )
      
      if (!(any(is.finite(stats_col[, 1, drop = TRUE])))) {
        stats_col[[1]] <- rep(NA_real_, 9)
      }
      
      summary_stats_tbl <- dplyr::bind_cols(summary_stats_tbl, stats_col)
    }
  }
  
  summary_stats_tbl
}

#' Table Transformer: obtain an information table for string columns
#' 
#' @description
#' With any table object, you can produce an information table that is scoped to
#' string-based columns. The table produced will have a leading column called
#' `".param."` with labels for each of the three rows, each corresponding to
#' the following pieces of information pertaining to string length:
#'
#' 1. Mean String Length (`"length_mean"`)
#' 2. Minimum String Length (`"length_min"`)
#' 3. Maximum String Length (`"length_max"`)
#'
#' Only string data from the input table will generate columns in the output
#' table. Column names from the input will be used in the output, preserving
#' order as well.
#' 
#' @param tbl A table object to be used as input for the transformation. This
#'   can be a data frame, a tibble, a `tbl_dbi` object, or a `tbl_spark` object.
#' 
#' @return A `tibble` object.
#' 
#' @examples 
#' # Get string information for the
#' # string-based columns in the
#' # `game_revenue` dataset
#' tt_string_info(game_revenue)
#' 
#' # Ensure that `player_id` and
#' # `session_id` values always have
#' # the same number of characters
#' # throughout the table
#' tt_string_info(game_revenue) %>%
#'   col_vals_equal(
#'     columns = vars(player_id),
#'     value = 15
#'   ) %>%
#'   col_vals_equal(
#'     columns = vars(session_id),
#'     value = 24
#'   )
#' 
#' # Check that the maximum string
#' # length in column `f` of the
#' # `small_table` dataset is no
#' # greater than `4`
#' tt_string_info(small_table) %>%
#'   col_vals_lte(
#'     columns = vars(f),
#'     value = 4
#'   )
#' 
#' @family Table Transformers
#' @section Function ID:
#' 12-2
#' 
#' @export
tt_string_info <- function(tbl) {
  
  # Determine whether the `tbl` object is acceptable here
  check_is_a_table_object(tbl = tbl)
  
  n_cols <- get_table_total_columns(data = tbl)
  
  tbl_info <- get_tbl_information(tbl = tbl)
  col_names <- tbl_info$col_names
  r_col_types <- tbl_info$r_col_types
  
  string_info_tbl <- 
    dplyr::tibble(
      .param. = c("length_mean", "length_min", "length_max")
    )
  
  for (i in seq_len(n_cols)) {
    
    if (r_col_types[i] == "character") {
      
      data_col <- dplyr::select(tbl, col_names[i])
      
      suppressWarnings({
        info_list <- get_table_column_nchar_stats(data_column = data_col)
      })
      
      info_col <- 
        tibble::enframe(
          unlist(info_list),
          name = NULL,
          value = col_names[i]
        )
      
      string_info_tbl <- dplyr::bind_cols(string_info_tbl, info_col)
    }
  }

  string_info_tbl
}

#' Table Transformer: get the dimensions of a table
#' 
#' @description
#' With any table object, you can produce an information table that contains
#' nothing more than the table's dimensions: the number of rows and the number
#' of columns.
#'
#' The table produced will have two columns and two rows. The first is the
#' `"dim"` column with the labels `"rows"` and `"columns"`; the second column,
#' `"value"`, contains the row and column counts.
#' 
#' @param tbl A table object to be used as input for the transformation. This
#'   can be a data frame, a tibble, a `tbl_dbi` object, or a `tbl_spark` object.
#' 
#' @return A `tibble` object.
#' 
#' @examples
#' # Get the dimensions of the
#' # `game_revenue` dataset that's
#' # included in the package
#' tt_tbl_dims(game_revenue)
#' 
#' # This output table is useful when
#' # you want to validate the
#' # dimensions of the table; here,
#' # we check that `game_revenue` has
#' # at least 1500 rows
#' tt_tbl_dims(game_revenue) %>%
#'   col_vals_gt(
#'     columns = vars(value),
#'     value = 1500,
#'     segments = dim ~ "rows"
#'   )
#' 
#' # We can check `small_table` for
#' # an exact number of columns (`8`)
#' tt_tbl_dims(small_table) %>%
#'   dplyr::filter(dim == "columns") %>%
#'   col_vals_equal(
#'     columns = vars(value),
#'     value = 8
#'   )
#' 
#' @family Table Transformers
#' @section Function ID:
#' 12-3
#' 
#' @export
tt_tbl_dims <- function(tbl) {
  
  # Determine whether the `tbl` object is acceptable here
  check_is_a_table_object(tbl = tbl)
  
  n_cols <- get_table_total_columns(data = tbl)
  n_rows <- get_table_total_rows(data = tbl)
  
  dplyr::tibble(
    dim = c("rows", "columns"),
    value = as.integer(c(n_rows, n_cols))
  )
}

#' Table Transformer: shift the times of a table
#' 
#' @description
#' With any table object containing date or date-time columns, these values can
#' be precisely shifted with `tt_time_shift()` and specification of the time
#' shift. We can either provide a string with the time shift components and the
#' shift direction (like `"-4y 10d"`) or a `difftime` object (which can be
#' created via **lubridate** expressions or by using the [base::difftime()]
#' function).
#' 
#' @details 
#' The `time_shift` specification cannot have a higher time granularity than the
#' least granular time column in the input table. Put in simpler terms, if there
#' are any date-based based columns (or just a single date-based column) then
#' the time shifting can only be in terms of years, months, and days. Using a
#' `time_shift` specification of `"20d 6H"` in the presence of any dates will
#' result in a truncation to `"20d"`. Similarly, a `difftime` object will be
#' altered in the same circumstances, however, the object will resolved to an
#' exact number of days through rounding.
#' 
#' @param tbl A table object to be used as input for the transformation. This
#'   can be a data frame, a tibble, a `tbl_dbi` object, or a `tbl_spark` object.
#' @param time_shift Either a character-based representation that specifies the
#'   time difference by which all time values in time-based columns will be
#'   shifted, or, a `difftime` object. The character string is constructed in
#'   the format `"0y 0m 0d 0H 0M 0S"` and individual time components can be
#'   omitted (i.e., `"1y 5d"` is a valid specification of shifting time values
#'   ahead one year and five days). Adding a `"-"` at the beginning of the
#'   string (e.g., `"-2y"`) will shift time values back.
#' 
#' @return A data frame, a tibble, a `tbl_dbi` object, or a `tbl_spark` object
#'   depending on what was provided as `tbl`.
#' 
#' @examples
#' # With the `game_revenue` dataset,
#' # which has entries in the first
#' # 21 days of 2015, move all of the
#' # date and date-time values to the
#' # beginning of 2021
#' tt_time_shift(
#'   tbl = game_revenue,
#'   time_shift = "6y"
#' )
#' 
#' # Keeping only the `date_time` and
#' # `a`-`f` columns of `small_table`,
#' # shift the times back 2 days and
#' # 12 hours
#' small_table %>%
#'   dplyr::select(-date) %>%
#'   tt_time_shift("-2d 12H")
#' 
#' @family Table Transformers
#' @section Function ID:
#' 12-4
#' 
#' @export
tt_time_shift <- function(tbl,
                          time_shift = "0y 0m 0d 0H 0M 0S") {
  
  # nocov start
  
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    
    stop(
      "The `tt_time_shift()` function requires the lubridate package:\n",
      "* It can be installed with `install.packages(\"lubridate\")`.",
      call. = FALSE
    )
  }
  
  # nocov end
  
  # Determine whether the `tbl` object is acceptable here
  check_is_a_table_object(tbl = tbl)

  tbl_info <- get_tbl_information(tbl = tbl)
  r_col_types <- tbl_info$r_col_types
  col_names <- tbl_info$col_names
  
  time_columns <- col_names[r_col_types %in% c("POSIXct", "Date")]
  
  if (length(time_columns) < 1) {
    return(tbl)
  }

  if (inherits(time_shift, "difftime")) {
    
    # If there is a date-based column in the table, we need to ensure
    # that the `difftime` object is rounded to a value of day units
    if ("Date" %in% r_col_types) {
      
      n_days <-
        as.integer(round(as.numeric(time_shift, units = "days"), digits = 0))
      
      tbl <- 
        tbl %>%
        dplyr::mutate(
          dplyr::across(
            .cols = time_columns,
            .fns = ~ lubridate::days(n_days) + .
          )
        )
      
    } else {
      
      tbl <- 
        tbl %>%
        dplyr::mutate(
          dplyr::across(
            .cols = time_columns,
            .fns = ~ time_shift + .
          )
        )
    }
    
  } else {
      
    if (grepl("^-", time_shift)) {
      direction_val <- -1L
      time_shift <- gsub("^-", "", time_shift)
    } else {
      direction_val <- 1L
    }
    
    difference_vec <- unlist(strsplit(time_shift, split = " "))
    
    for (i in seq_along(difference_vec)) {
      
      if (grepl("[0-9]+?(y|m|d|H|M|S)$", difference_vec[i])) {
        
        time_basis <- gsub("[0-9]+?(y|m|d|H|M|S)", "\\1", difference_vec[i])
        time_value <- 
          as.integer(gsub("([0-9]+?)(y|m|d|H|M|S)", "\\1", difference_vec[i]))
        
        # If the time value is zero, then proceed to the next iteration
        if (time_value == 0) next
        
        # Don't shift times by hours, minutes, or seconds if there are any
        # time columns that are date-based (this will either fail or yield
        # undesirable time values in date-based columns)
        if (time_basis %in% c("H", "M", "S") && "Date" %in% r_col_types) next
        
        fn_time <-
          switch(
            time_basis,
            y = lubridate::years,
            m = lubridate::dmonths,
            d = lubridate::days,
            H = lubridate::hours,
            M = lubridate::minutes,
            S = lubridate::seconds
          )
        
        # Apply the time change for the particular time basis to all columns
        tbl <- 
          tbl %>%
          dplyr::mutate(
            dplyr::across(
              .cols = time_columns,
              .fns = ~ fn_time(time_value * direction_val) + .)
          )
      }
    }
  }
  
  tbl
}

#' Table Transformer: slice a table with a slice point on a time column
#' 
#' @description
#' With any table object containing date, date-time columns, or a mixture
#' thereof, any one of those columns can be used to effectively slice the data
#' table in two with a `slice_point`: and you get to choose which of those
#' slices you want to keep. The slice point can be defined in several ways. One
#' method involves using a decimal value between `0` and `1`, which defines the
#' slice point as the time instant somewhere between the earliest time value (at
#' `0`) and the latest time value (at `1`). Another way of defining the slice
#' point is by supplying a time value, and the following input types are
#' accepted: (1) an ISO 8601 formatted time string (as a date or a date-time),
#' (2) a `POSIXct` time, or (3) a `Date` object.
#' 
#' @details 
#' There is the option to `arrange` the table by the date or date-time values in
#' the `time_column`. This ordering is always done in an ascending manner. Any
#' `NA`/`NULL` values in the `time_column` will result in the corresponding rows
#' can being removed (no matter which slice is retained).
#'  
#' @param tbl A table object to be used as input for the transformation. This
#'   can be a data frame, a tibble, a `tbl_dbi` object, or a `tbl_spark` object.
#' @param time_column The time-based column that will be used as a basis for the
#'   slicing. If no time column is provided then the first one found will be
#'   used.
#' @param slice_point The location on the `time_column` where the slicing will
#'   occur. This can either be a decimal value from `0` to `1`, an ISO 8601
#'   formatted time string (as a date or a date-time), a `POSIXct` time, or a
#'   `Date` object.
#' @param keep Which slice should be kept? The `"left"` side (the default)
#'   contains data rows that are earlier than the `slice_point` and the
#'   `"right"` side will have rows that are later.
#' @param arrange Should the slice be arranged by the `time_column`? This may be
#'   useful if the input `tbl` isn't ordered by the `time_column`. By default,
#'   this is `FALSE` and the original ordering is retained.
#' 
#' @return A data frame, a tibble, a `tbl_dbi` object, or a `tbl_spark` object
#'   depending on what was provided as `tbl`.
#' 
#' @examples
#' # With the `game_revenue` dataset,
#' # which has entries in the first
#' # 21 days of 2015, elect to get all
#' # of the records where the `time`
#' # values are strictly for the first
#' # 15 days of 2015
#' tt_time_slice(
#'   tbl = game_revenue,
#'   time_column = "time",
#'   slice_point = "2015-01-16"
#' )
#' 
#' # Omit the first 25% of records
#' # from `small_table` on the basis
#' # of a timeline that begins at 
#' # `2016-01-04 11:00:00` and
#' # ends at `2016-01-30 11:23:00`
#' small_table %>%
#'   tt_time_slice(
#'     slice_point = 0.25,
#'     keep = "right"
#'   )
#' 
#' @family Table Transformers
#' @section Function ID:
#' 12-5
#' 
#' @export
tt_time_slice <- function(tbl,
                          time_column = NULL,
                          slice_point = 0,
                          keep = c("left", "right"),
                          arrange = FALSE) {
  
  # nocov start
  
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    
    stop(
      "The `tt_time_shift()` function requires the lubridate package:\n",
      "* It can be installed with `install.packages(\"lubridate\")`.",
      call. = FALSE
    )
  }
  
  # nocov end
  
  keep <- match.arg(keep)
  
  # Determine whether the `tbl` object is acceptable here
  check_is_a_table_object(tbl = tbl)
  
  tbl_info <- get_tbl_information(tbl = tbl)
  r_col_types <- tbl_info$r_col_types
  col_names <- tbl_info$col_names
  
  all_time_columns <- col_names[r_col_types %in% c("POSIXct", "Date")]
  
  if (length(all_time_columns) < 1) {
    return(tbl)
  }
  
  if (is.null(time_column)) {
    time_column <- all_time_columns[1]
  }
  
  col_sym <- rlang::sym(time_column)
  
  time_bounds <-
    tbl %>%
    dplyr::select(!!col_sym) %>%
    dplyr::summarize_all(
      .funs = list(
        ~ min(., na.rm = TRUE),
        ~ max(., na.rm = TRUE)
      )
    ) %>%
    dplyr::collect() %>%
    as.list()
  
  if (is.numeric(slice_point)) {
    
    if (slice_point < 0 || slice_point > 1) {
      stop(
        "When provided as a number, `slice_point` must be between 0 and 1",
        call. = FALSE
      )
    }
    
    time_range_s <-
      as.numeric(
        difftime(
          time1 = time_bounds$max,
          time2 = time_bounds$min,
          units = "secs"
        )
      )
    
    time_slice_instant <-
      time_bounds$min +
      lubridate::seconds(time_range_s * slice_point)
    
  } else if (inherits(slice_point, "character")) {
    
    if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", slice_point[1])) {
      
      time_slice_instant <-
        lubridate::ymd_hms(paste(slice_point[1], "00:00:00"))
      
    } else if (
      grepl(
        "^[0-9]{4}-[0-9]{2}-[0-9]{2}(T| )[0-9]{2}:[0-9]{2}:[0-9]{2}$",
        slice_point[1]
        )) {
      
      time_slice_instant <- lubridate::ymd_hms(slice_point[1])
      
    } else {
      
      stop(
        "The `slice_point` must be a date or date-time",
        call. = FALSE
        )
    }
    
  } else if (inherits(slice_point, "POSIXct")) {
    
    time_slice_instant <- slice_point
    
  } else if (inherits(slice_point, "Date")) {
    
    time_slice_instant <- slice_point
  }
  
  # Optionally arrange rows by the time column
  if (arrange) {
    tbl <- dplyr::arrange(tbl, !!col_sym)
  }
  
  # Perform the filtering of data either on the left or right of
  # the `time_slice_instant`
  if (keep == "left") {
    tbl <- dplyr::filter(tbl, !!col_sym < time_slice_instant)
  } else {
    tbl <- dplyr::filter(tbl, !!col_sym >= time_slice_instant)
  }
  
  tbl
}