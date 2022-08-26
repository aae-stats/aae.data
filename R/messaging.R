# internal function to return a message if all data checks are OK for a field
pass_message <- function(input, match, data_type) {
  
  # check whether the input and matched fields are the same  
  check <- check_field(input, match)
  
  # set message
  message <- paste0(
    "**[", input, "]** (", data_type, ") is in the correct format. ", check, "\n\n"
  )

  # return
  list(message = message, colour = "green")
  
}

# internal function to return a message if variable is of unknown data type
new_type_message <- function(input) {
  list(
    message = paste0("**[", input, "] is a new data type and hasn't been assessed** \n\n"),
    colour = "blue"
  )
}

# internal function to return a message if variable does not pass checks
fail_message <- function(input, match, data_type, issue) {

  # check whether the input and matched fields are the same  
  check <- check_field(input, match)

  # set message
  message <- paste0(
    "**[", input, "]** (", data_type, ") has ", issue, " values. ", check, "\n\n"
  )
  
  # return
  list(message = message, colour = "red")
  
}

# internal function to return a message if coordinate does not pass checks
coordinate_message <- function(input, match, issue) {
  
  # check whether the input and matched fields are the same  
  check <- check_field(input, match)
  
  # set message
  message <- paste0(
    "**[", input, "]** (coordinate) ", issue, ". ", check, "\n\n"
  )
  
  # return
  list(message = message, colour = "red")
  
}

# internal function to return a message if a non-standard value 
#    of a restricted field is included
non_standard_message <- function(input, match, data_type, issue, link) {
  
  # check whether the input and matched fields are the same  
  check <- check_field(input, match)
  
  # set message
  message <- paste0(
    "**[", input, "]** (", data_type, ") is required and has ", issue, ". Please update to ", link, ". ", check, "\n\n"
  )
  
  # return
  list(message = message, colour = "red")
  
}

# internal function to return a message if a duplicate site
#    code or description is included
duplicate_message <- function(issue) {
  
  # set message
  colour <- "red"
  if (issue == "none") {
    message <- paste0("Site descriptions appear to be valid and do not contain duplicate or inconsistent values. \n\n")
    colour <- "green"
  }
  if (issue == "multiple_description")
    message <- paste0("**NOTE:** Some sites have multiple (different) descriptions. \n\n")
  if (issue == "same_description")
    message <- paste0("**NOTE:** The same description occurs for multiple sites. \n\n")
  if (issue == "same_multiple_description")
    message <- paste0("**NOTE:** The same description occurs for multiple sites and some sites have multiple descriptions. \n\n")
  
  # return
  list(message = message, colour = colour)
  
}

# internal function to print a message outlining a basic data summary
summary_message <- function(input, match) {

  # check if input exists
  if (any(is.na(input))) {
    
    # create a message if not
    message <- paste0(
      "**Data set does not contain a ", match, " field. \n\n"
    )
    
  } else {
    
    # simple if a single field
    if (length(input) == 1) {
    
      # check input and matched field
      check <- check_field(input, match)
      
      # create message
      message <- paste0(
        "**Data set contains the following values or range of values in the ", input, " field. ", check, "\n\n"
      )
      
    } else {
      
      # error if more than two values
      if (length(input) > 2)
        stop("input should have one or two values", call. = FALSE)
      
      # otherwise need to combine two field names (typically coordinates)
      check <- c(
        check_field(input[1], match[1]),
        check_field(input[2], match[2])
      )
      
      # tidy these up
      if (any(check != "")) {
        check <- paste0("Matched to standard names: **[", match[1], "]** and **[", match[2], "]**")
      } else {
        check <- ""
      }
      
      # and tidy up the input names
      input <- paste0(input[1], " and ", input[2])
      
      # create message
      message <- paste0(
        "**Data set contains the following values or range of values in the ", input, " fields. ", check, "\n\n"
      )
      
      
    }
    
  }
  
  # return
  list(message = message, colour = "green")
  
}

missing_fields <- function(x, link) {
  
  if (any(x$missing_required)) {
    
    message <- paste0(
      "Please check all column names and use the ",
      "[standardised AAE column names](",
      link,
      ") to ensure correct matches. \n\n"
    )
    colour <- "red"
    
  } else {
    
    message <- "All required information has been provided. \n\n"
    colour <- "green"
    
  }
  
  # return
  list(
    message = message,
    colour = colour
  )
  
}

# helper function to check if a field has been renamed
check_field <- function(input, match) {
  msg <- ""
  if (input != match)
    msg <- paste0("Matched to standard name: **[", match, "]** ")
  msg
}
