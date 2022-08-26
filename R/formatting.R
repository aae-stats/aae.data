# internal function to clean column names, removing
#   leading or trailing "X", "." and replacing intermediate
#   "." with "_"
clean_columns <- function(x) {
  colnames(x) <- gsub("^X\\.|^\\.|\\.+$", "", colnames(x))
  colnames(x) <- gsub("\\.+" , "_", colnames(x))
  x[, colnames(data) != ""]
}

# internal function to check and format dates
#' @import lubridate
format_dates <- function(x, validate, orders = c("dmy", "ymd", "dmy_HMS", "ymd_HMS")) {
  
  # list all date fields in the input data set that have associated data
  date_fields <- validate %>% filter(data_type == "date") %>% na.omit()
  
  # how many date types are there?
  ndate <- length(date_fields$xlsx_fields)
  
  # loop over these, formatting each in turn and recording the output
  #   as a new field
  for (i in seq_len(ndate)) {
    x <- x %>%
      mutate(
        tmp_date = parse_date_time(
          data[, date_fields$xlsx_fields[i]],
          orders = orders
        )
      ) %>%
      rename_with( ~ paste0(date_fields$xlsx_fields[i], "_formatted"), tmp_date)
  }
  
  # return full data set with formatted dates
  x
  
}

# function to paste strings with correct use commas and "and"
tidy_paste <- function(x) {
  
  # blank x means there was no fuzzy match
  if (length(x) == 0)
    out <- "<None available>"
  
  # return x if it's only a single value
  if (length(x) == 1)
    out <- x
  
  # and split by "and" if it's two values
  if (length(x) == 2)
    out <- paste0(x, collapse = " or ")
  
  # or use Oxford comma otherwise
  if (length(x) > 2)
    out <- paste0(c(paste0(x[seq_len(length(x) - 1)], collapse = ", "), x[length(x)]), collapse = ", or ")
  
  # return
  out
  
}

# function to find partial matches from a lookup table and
#   flatten these into a single character vector
partial_match <- function(provided, available, max_distance = 2) {
  
  # initialise
  out <- rep(NA, length(provided))
  
  # check each element of provided and return
  #    fuzzy matched fields
  for (i in seq_along(provided)) {
    idx <- agrepl(provided[i], available, max.distance = max_distance)
    out[i] <- tidy_paste(available[idx])
  }
  
  # return
  out
  
}

# internal function to set coloured text based on output type
#' @import knitr
colourise <- function(x, colour) {
  
  # default is to do nothing
  out <- x
  
  # set colour appropriately for latex outpt
  if (knitr::is_latex_output())
    out <- sprintf("\\textcolor{%s}{%s}", colour, x)
  
  # or HTML output
  if (knitr::is_html_output())
    out <- sprintf("<span style='color: %s;'>%s</span>", colour, x)
  
  # return
  out
  
}