# function to list all waterbodies inlcuded in a data set
summarise_field <- function(x, val, field) {
  
  # check if field is included in data set
  available <- all(field %in% val$fields)
  
  # summarise if available
  if (available) {
    
    # work out relevant field in input data
    input <- val %>% filter(fields %in% field) %>% pull(xlsx_fields)
    
    # grab formatted dates if dealing with a date
    if (any(field == "survey_date"))
      input <- paste0(input, "_formatted")
    
    # create a table of unique values
    #   (table because some checks consider multiple fields)
    x <- x %>%
      select(all_of(input)) %>%
      distinct()
    
  } else {
    
    # set some default outputs
    input <- NA
    x <- NA
    
  }
  
  # and create an object containing an informative message
  message <- summary_message(
    input = input,
    match = field
  )
  
  # return
  list(
    message = message,
    table = x,
    field = field
  )
  
}

# function to check total_length, fork_length, weight by species
##  calculate min, max values
summarise_measurements <- function(x, val, field) {
  
  # work out relevant field in input data
  sp_input <- val %>% filter(fields == "species") %>% pull(xlsx_fields)
  input <- val %>%
    filter(fields == field) %>% 
    pull(xlsx_fields)
  
  # need some values otherwise this is all unnecessary
  if (length(input) == 0) {

    input <- NA
    x <- NA
    
  } else {
    
    # create a table of unique values, group by species
    #   if available
    if (length(sp_input) == 0) {
      x <- x %>% select(all_of(input))
    } else {
      x <- x %>% 
        select(all_of(sp_input), all_of(input)) %>%
        group_by(across(1))
    }
    
    # calculate weight range for input variable
    x <- x %>%
      summarise(
        across(all_of(input),
               list(
                 min = ~ suppressWarnings(min(.x, na.rm = TRUE)),
                 max = ~ suppressWarnings(max(.x, na.rm = TRUE))
               )
        )
      ) %>%
      distinct()
    
  }
  
  # and create an object containing an informative message
  message <- summary_message(
    input = input,
    match = field
  )
  
  # return
  list(
    message = message,
    table = x,
    field = field
  )
  
}
