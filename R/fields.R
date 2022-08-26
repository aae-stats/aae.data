# functions to extract and match input fields with database requirements

# imports used below
#' @import dplyr

# function to find matched fields based on a lookup table
find_matched <- function(x) {

  # return data.frame of  all fields in input data
  x <- data.frame(fields = colnames(x))
  
  # grab the field codes from lookup table
  fields_lu <- fetch_fields()
  
  # and return joined tables
  x %>%
    left_join(fields_lu, by = c("fields" = "in_col")) %>%
    mutate(
      matched = !is.na(out_col),
      unmatched = is.na(out_col),
      xlsx_fields = fields
    ) %>%
    select(-fields)
  
}

# function to get standard fields from lookup table
get_standard <- function(x) {
  
  # grab a lookup table of standard fields
  standard_fields_lu <- fetch_standard_fields()
  
  # match with provided fields and return full list
  standard_fields_lu %>%
    full_join(x, by = c("fields" = "out_col"))
  
}

# function to check whether required fields are provided
check_required <- function(x) {
  
  # identify any required fields and return full table
  x %>%
    filter(required == 1) %>% 
    mutate(
      fields = ifelse(is.na(xlsx_fields), paste0("**", fields, "**"), fields),
      missing_required = is.na(xlsx_fields)
    )
  
}

# function to pull out recognised fields
extract_validated <- function(x) {
  x %>% filter(!is.na(xlsx_fields), !is.na(fields), !is.na(data_type))
}

# function to list unmatched fields
get_unmatched <- function(x) {
  
  # grab the field codes from lookup table
  fields_lu <- fetch_fields()
  
  # pull out unmatched fields
  unmatched_fields <- x %>% filter(unmatched)
  
  # check for partial matches
  unmatched_fields <- unmatched_fields %>%
    mutate(
      did_you_mean = partial_match(xlsx_fields, unique(fields_lu$out_col), max_distance = 0.2)
    )
  
  # and return
  unmatched_fields
  
}
