# QA/QC functions for each data type

# uses many dplyr functions
#' @import dplyr

# wrapper function to validate data based on data type
validate_data <- function(x, val, mga = NULL) {
  
  # extract field names and null requirements
  input <- val[["xlsx_fields"]]
  match <- val[["fields"]]
  no_null <- val[["no_nulls"]]
  
  # pull out input fields from validation data  
  x <- x %>%
    mutate(
      test = x %>% pull(all_of(input)),
      test = as.character(test),
      test = ifelse(test == "" | is.na(test), "No value", test)
    )
  
  # run QA/QC based on data type
  qaqc <- switch(
    val[["data_type"]],
    "numeric" = numeric_qaqc(x, input, match, no_null),
    "string" = string_qaqc(x, input, match, no_null),
    "date" = date_qaqc(x, input, match, no_null),
    "coordinate" = coordinate_qaqc(x, input, match, no_null, mga = mga),
    new_type_qaqc(x, input, match, no_null)
  )
  
  # return
  qaqc
  
}

# internal function to check data with unknown format
new_type_qaqc <- function(data, input, match, no_null, verbose = TRUE){
  
  # simple message stating that data type is unknown
  list(
    message = new_type_message(input = input),
    missing = NULL,
    invalid = NULL,
    unexpected = NULL,
    errors = TRUE,
    type = "new_data_type"
  )
  
}

# internal function to check numeric data
numeric_qaqc <- function(data, input, match, no_null){
  
  # assume all good unless errors found below
  errors <- FALSE
  missing <- NULL
  invalid <- NULL
  unexpected <- NULL
  
  # convert factors to numeric if required  
  if (is.factor(data$test)) {
    data$numeric_test <- suppressWarnings(as.numeric(levels(data$test))[data$test])
  } else {
    data$numeric_test <- suppressWarnings(as.numeric(data$test))
  }
  
  # count entries that do not convert to numeric values
  test_result <- data %>%
    select(test, numeric_test) %>%
    filter(is.na(numeric_test)) %>%
    group_by(test, numeric_test) %>%
    summarise(count = n(), .groups = "drop")
  
  # print a list of values with issues and their count
  if (nrow(test_result) > 0) {
    
    # return a message about the offending variable
    message <- fail_message(input, match, "numeric", issue = "incorrect")
    
    # and give details
    invalid <- test_result %>% select(test, count)
    
    # update errors flag
    errors <- TRUE
    
  }
  
  # check if all values are entered for data types requiring complete data
  if (no_null == 1 & any(grepl("No value", data$test))) {
    
    # print a message about the offending variable
    message <- fail_message(input, match, "numeric", issue = "missing")
    if (errors)
      message <- fail_message(input, match, "numeric", issue = "missing and incorrect")
    
    # and give details
    missing <- data %>% 
      filter(test == "No value") %>% 
      group_by(test) %>% 
      summarise(count = n())
    
    # update errors flag
    errors <- TRUE
    
  }
  
  # check for unexpected negative values
  non_negative_fields <- c(
    "electro_seconds",
    "total_ef_seconds",
    "depth_min",
    "depth_max",
    "depth_avg",
    "collected",
    "observed",
    "age",
    "weight",
    "fork_length",
    "total_length",
    "pass_number"
  )
  if (match %in% non_negative_fields) {
    
    # check for negative values
    if (any(data$test < 0)) {
      
      # write an informative error
      issue <- "negative"
      if (errors) {
        issue <- "incorrect and negative"
        if (grepl("missing", message$message))
          issue <- "missing and negative"
        if (grepl("missing and incorrect", message$message))
          issue <- "missing, incorrect, and negative"
      }
      message <- fail_message(input, match, "numeric", issue = issue)
      
      # and give details
      unexpected <- data %>% 
        mutate(
          sign = ifelse(test < 0, "negative", "non_negative")
        ) %>%
        filter(sign == "negative") %>% 
        group_by(sign) %>%
        summarise(count = n())
      
      # update errors flag
      errors <- TRUE
      
    }
    
  }
  
  # otherwise everything is all good
  if (!errors)
    message <- pass_message(input, match, "numeric")
  
  # return
  list(
    message = message,
    missing = missing,
    invalid = invalid,
    unexpected = unexpected,
    errors = errors,
    type = "numeric"
  )
  
}

# internal function to check character data
string_qaqc <- function(data, input, match, no_null) {
  
  # set default errors flag
  errors <- FALSE
  missing <- NULL
  invalid <- NULL
  unexpected <- NULL
  
  # count number of non-NA values in test
  test_result <- data %>%
    select(test) %>%
    filter(test == "No value") %>%
    group_by(test) %>%
    summarise(count = n())
  
  # check values are complete for variables that require full data
  if (no_null == 1) {
    
    # check for missing values (not allowed is no_null == 1)
    if (any(grepl("No value", data$test))) {
      
      # return message about the problem variable
      message <- fail_message(input, match, "text", issue = "missing")
      
      # and give details
      missing <- test_result
      
      # update errors flag
      errors <- TRUE
      
    }
    
  }
  
  # check for non-standard gear types
  if (match == "gear_type") {
    
    # grab the look-up for gear types
    gear_type_lu <- fetch_gear_types()
    
    # match this to the data set
    data <- data %>% mutate(test_upper = toupper(test))
    gear <- data %>%
      left_join(gear_type_lu, by = c("test_upper" = "gear_type")) %>%
      select(test, gear_desc) %>%
      filter(is.na(gear_desc)) %>%
      group_by(test) %>%
      summarise(count = n()) %>%
      arrange(test)
    
    # there is an issue if there are any NA gear descriptions
    if (nrow(gear) > 0) {
      
      # return a message about non-standard gear type if required
      link <- "[**AAE standard gear type values**](https://delwpvicgovau.sharepoint.com/:x:/r/sites/ecm_92/AAEAdministration/AAE%20Database%20and%20Standards/DRAFT_aae_standard_data_entry_attributes.xlsx?d=w537521fd2de044aeb9dd71a0ac804af4&csf=1&web=1&e=iqXs2t)"
      message <- non_standard_message(input, match, "text", issue = "non-standard values", link = link)
      if (errors)
        message <- non_standard_message(input, match, "text", issue = "missing values and non-standard gear type", link = link)
      
      # and print out a table with this info
      unexpected <- gear
      
      # update errors flag
      errors <- TRUE
      
    } 
    
  } 
  
  # check for non-standard species names
  if (match == "species") {
    
    # grab the look-up for gear types
    species_lu <- fetch_vba_species()
    
    # match this to the data set
    data <- data %>% mutate(test_lower = tolower(test))
    spp <- data %>%
      left_join(species_lu, by = c("test_lower" = "species")) %>%
      select(test_lower, test, vba_taxon_id) %>%
      filter(is.na(vba_taxon_id), test_lower != "no fish") %>%
      group_by(test) %>%
      summarise(count = n()) %>%
      arrange(test)
    
    # there is an issue if there are any NA VBA taxon IDs
    if (nrow(spp) > 0) {
      
      # print a message about non-standard gear type if required
      link <- "[**VBA species names**](https://delwpvicgovau.sharepoint.com/:x:/r/sites/ecm_92/AAEAdministration/AAE%20Database%20and%20Standards/DRAFT_aae_standard_data_entry_attributes.xlsx?d=w537521fd2de044aeb9dd71a0ac804af4&csf=1&web=1&e=iqXs2t)"
      message <- non_standard_message(input, match, "text", issue = "non-VBA species names", link = link)
      if (errors)
        message <- non_standard_message(input, match, "text", issue = "missing values and non-VBA species names", link = link)
      
      # and print out a table with this info
      unexpected <- spp
      
      # update errors flag
      errors <- TRUE
      
    }           
    
  }
  
  # everything is OK otherwise
  if (!errors)
    message <- pass_message(input, match, "text")
  
  # return
  list(
    message = message,
    missing = missing,
    invalid = invalid,
    unexpected = unexpected,
    errors = errors,
    type = "text"
  )
  
}

# internal function to check for duplicate fields representing the
#    same thing
check_duplicates <- function(data, validate) {
  
  # set default errors flag
  errors <- FALSE
  missing <- NULL
  invalid <- NULL
  unexpected <- NULL
  
  # check for multiple site descriptions per site
  if (any(grepl("site_desc", validate$fields))) {
    
    # pull out site codes and site descriptions
    site_code <- validate %>% 
      filter(fields == "site_code") %>% 
      pull(xlsx_fields)
    site_desc <- validate %>% 
      filter(fields == "site_desc") %>% 
      pull(xlsx_fields)
    
    # split up by waterbody if known
    if (any(grepl("waterbody", validate$fields))) {
      
      # collate waterbody and site info
      waterbody_field <- validate %>%
        filter(fields == "waterbody") %>%
        pull(xlsx_fields)
      
      # pull out distinct entries and check for multiple descriptions per site
      site_counts <- data %>% 
        select(all_of(waterbody_field), all_of(site_code), all_of(site_desc)) %>%
        distinct()
      duplicate_descriptions <- site_counts %>%
        select(all_of(waterbody_field), all_of(site_code)) %>%
        group_by(all_of(waterbody_field), all_of(site_code)) %>%
        summarise(count = n()) %>%
        filter(count > 1)
      
      # print an error if duplicates exist
      if (nrow(duplicate_descriptions) > 0) {
        
        # return a warning message
        message <- duplicate_message(issue = "multiple_description")
        
        # and a table of duplicates
        unexpected <- duplicate_descriptions
        
        # update errors flag
        errors <- TRUE
        
      } 
      
      # check for distinct sites with the same description
      duplicate_sites <- site_counts %>%
        select(all_of(waterbody_field), all_of(site_desc)) %>%
        group_by(all_of(waterbody_field), all_of(site_desc)) %>%
        summarise(count = n()) %>%
        filter(count > 1)
      
      # return an error if duplicates exist
      if (nrow(duplicate_sites) > 0) {
        
        # return a message
        message <- duplicate_message(issue = "same_description")
        if (errors)
          message <- duplicate_message(issue = "same_multiple_description")
        
        # and save a table of issues, adding to multiple description sites
        #   if these exist
        unexpected <- rbind(unexpected, duplicate_sites)
        
        # update errors flag
        errors <- TRUE
        
      } 
      
    } else {
      
      # pull out distinct entries and check for multiple descriptions per site
      site_counts <- data %>% 
        select(all_of(site_code), all_of(site_desc)) %>%
        distinct()
      duplicate_descriptions <- site_counts %>%
        select(all_of(site_code)) %>%
        group_by(all_of(site_code)) %>%
        summarise(count = n()) %>%
        filter(count > 1)
      
      # print an error if duplicates exist
      if (nrow(duplicate_descriptions) > 0) {
        
        # return a warning message
        message <- duplicate_message(issue = "multiple_description")
        
        # and a table of duplicates
        unexpected <- duplicate_descriptions
        
        # update errors flag
        errors <- TRUE
        
      } 
      
      # check for distinct sites with the same description
      duplicate_sites <- site_counts %>%
        select(all_of(site_desc)) %>%
        group_by(all_of(site_desc)) %>%
        summarise(count = n()) %>%
        filter(count > 1)
      
      # print an error if duplicates exist
      if (nrow(duplicate_sites) > 0) {
        
        # return a message
        message <- duplicate_message(issue = "same_description")
        if (errors)
          message <- duplicate_message(issue = "same_multiple_description")
        
        # and save a table of issues, adding to multiple description sites
        #   if these exist
        unexpected <- rbind(unexpected, duplicate_sites)
        
        # update errors flag
        errors <- TRUE
        
      } 
      
    }   
    
  }
  
  # everything is OK otherwise
  if (!errors)
    message <- duplicate_message(issue = "none")
  
  # return
  list(
    message = message,
    missing = missing,
    invalid = invalid,
    unexpected = unexpected,
    errors = errors,
    type = "text"
  )
  
}

# internal function to check character data
date_qaqc <- function(data, input, match, no_null) {
  
  # set default errors flag
  errors <- FALSE
  missing <- NULL
  invalid <- NULL
  unexpected <- NULL
  
  # need to extract the formatted version of the date field
  date_field <- paste0(input, "_formatted")
  
  # count number of non-NA values in test
  test_result <- data %>%
    select(test, all_of(date_field)) %>%
    filter(is.na(all_of(date_field))) %>%
    group_by(test, all_of(date_field)) %>%
    summarise(count = n())
  
  # error if there are NAs in date field
  if (nrow(test_result) > 0) {
    
    # print a message about the offending variable
    message <- fail_message(input, match, "date", issue = "incorrect")
    
    # and give details
    invalid <- test_result %>% select(test, count)
    
    # update errors flag
    errors <- TRUE
    
  }
  
  # everything is OK otherwise
  if (!errors)
    message <- pass_message(input, match, "date")
  
  # return
  list(
    message = message,
    missing = missing,
    invalid = invalid,
    unexpected = unexpected,
    errors = errors,
    type = "date"
  )
  
}

# internal function to restrict coordinates to Victoria
#  TODO: update with better method (e.g. intersection )
test_coordinate <- function(x, type, zone = NULL) {
  
  # specify coordinates based on zone if required
  if (!is.null(zone)) {
    if (zone == "55") {
      northing_bounds <- c(5548706, 6341600)
      easting_bounds <- c(168839, 814627)
    }
    if (zone == "54") {
      northing_bounds <- c(5524051, 6348270)
      easting_bounds <- c(425179, 860518)
    }
    # if (zone == "vicgrid") {
    #   northing_bounds <- c(2150719, 2931756)
    #   easting_bounds <- c(2031902, 3061604)
    # }
  }
  
  # set bounds based on coordinate type
  bounds <- switch(
    type,
    "longitude" = c(140, 151),
    "latitude" = c(-40, -33),
    "northing" = northing_bounds,
    "easting" = easting_bounds
  )
  
  # and return -999 if values are outside of these bounds
  #   (preserve NAs because missing is not the same error)
  ifelse(x >= bounds[1] & x <= bounds[2], x, -999)
  
}

# internal function to check coordinate data to ensure it is valid
#    and is within the state of Victoria
coordinate_qaqc <- function(data, input, match, no_null, mga = NULL){
  
  ## TODO: 
  # if not numeric
  #  - remove characters and keep spacing
  #  - check for space padding and remove
  #  - check if degrees decimal minutes
  #  - convert to decimal degrees
  #  - convert to numeric
  
  # set error flag
  errors <- FALSE
  missing <- NULL
  invalid <- NULL
  unexpected <- NULL
  
  # check if there are basic issues in the numeric input
  #   (this will catch character coordinates for now)
  errors_numeric <- numeric_qaqc(
    data = data,
    input = input,
    match = match,
    no_null = no_null
  )
  
  # how many rows in the full data set (stored for printed outputs)
  n_total <- nrow(data)
  
  # focus on numerically valid records
  data <- data %>%
    mutate(coord_num = as.numeric(test)) %>%
    filter(!is.na(coord_num))
  
  # how rows in this reduced data set?
  n_sub <- nrow(data)
  
  # check validity if numeric coordinates are provided
  if (n_sub > 0) {
    
    # work out range of values
    min_coord <- min(data$coord_num)
    max_coord <- max(data$coord_num)
    
    # use these to work out coordinate type
    coord_type <- "mga"
    if (min_coord >= -90 & max_coord <= 180)
      coord_type <- "lat_lon"
    if ((min_coord < 0 & max_coord >= 0) | (min_coord <= 180 & max_coord > 180))
      coord_type <- "mixed"
    
    if (coord_type == "lat_lon") {
      
      # replace values with -999 if they sit well beyond Victoria
      if (match %in% c("x_coordinate", "section_start_x", "section_end_x")) {
        data$coord_num <- test_coordinate(data$coord_num, type = "longitude")
        range <- "140** to **151"
      }
      if (match %in% c("y_coordinate", "section_start_y", "section_end_y")) {
        data$coord_num <- test_coordinate(data$coord_num, type = "latitude")
        range <- "-40** to **-33"
      }
      
      # count up the NA values
      test_result <- data %>%
        filter(coord_num == -999) %>%
        # filter(is.na(coord_num)) %>%
        group_by(test, coord_num) %>%
        summarise(count = n(), .groups = "drop")
      
      # and create a sumary output if NAs occur
      if (nrow(test_result) > 0) {
        
        # store an error message
        message <- coordinate_message(input, match, paste0("has values outside of the expected range (**", range, "**). Check that coordinats are correctly labelled as latitude (Y) or longitude (X) and are within the general vicinity of Victoria"))
        
        # and record the unexpeted values
        unexpected <- test_result %>%
          select(test, count)
        
        # update errors flag
        errors <- TRUE
        
      } 
      
    } 
    
    # there is an issue if coordinates have mixture of positive/negative values or
    #   values above and below 180
    if (coord_type == "mixed") {
      
      # store an error message
      message <- coordinate_message(input, match, "might have a mixture of latitude/longitude or northing/easting values")
      
      # update errors flag
      errors <- TRUE
      
    }
    
    # assume mga coordinates otherwise
    #   TODO: update this to check coordinate type more reliably
    if (coord_type == "mga") {
      
      # zone is required for MGA coordinates
      if (is.null(mga)) {
        
        # return an error if zone not provided
        message <- coordinate_message(input, match, "appears to contain northing/easting values but mga_zone is not provided")
        
        # and update errors flag
        errors <- TRUE
        
      } else {
        
        # work out the MGA zone and check coordinates are within the appropriate ranges
        mga_zone <- data %>% pull(all_of(mga)) %>% as.character()
        
        # currently accepted zones are 54, 55, or "vicgrid", return an error if 
        #   this is not the case
        if (!all(unique(mga_zone) %in% c("54", "55", "vicgrid"))) {
          
          # return an error if mga_zone includes non-accepted zones
          message <- coordinate_message(input, match, "appears to contain northing/easting values but MGA zones are not an accepted value (54 or 55)")
          
          # and update errors flag
          errors <- TRUE
          
        }
        
      }
      
      if (!errors) {
        
        # replace values with -999 if they sit well beyond Victoria
        unique_mga <- unique(mga_zone)
        if (match == "x_coordinate") {
          for (i in seq_along(unique_mga)) {
            idx <- mga_zone == unique_mga[i]
            data$coord_num[idx] <- test_coordinate(data$coord_num, type = "easting", zone = unique_mga[i])
          }
          range <- "168839** to **814627** for **zone 55** and **425179** to **860518** for **zone 54"
        }
        if (match == "y_coordinate") {
          for (i in seq_along(unique_mga)) {
            idx <- mga_zone == unique_mga[i]
            data$coord_num[idx] <- test_coordinate(data$coord_num, type = "northing", zone = unique_mga[i])
          }
          range <- "5548706** to **6341600** for **zone 55** and **5524051** to **6348270** for **zone 54"
        }
        
        # count up the NA values
        test_result <- data %>%
          filter(coord_num == -999) %>%
          group_by(test, coord_num) %>%
          summarise(count = n(), .groups = "drop")
        
        # and create a summary output if NAs occur
        if (nrow(test_result) > 0) {
          
          # store an error message
          message <- coordinate_message(input, match, paste0("has values outside of the expected range (**", range, "**). Check that coordinats are correctly labelled as northing (Y) or easting (X), have a correctly attributed MGA zone, and are within the general vicinity of Victoria"))
          
          # and record the unexpeted values
          unexpected <- test_result %>%
            select(test, count)
          
          # update errors flag
          errors <- TRUE
          
        } 
        
      }
      
    }
    
  }
  
  # default to numeric error message if no errors
  #   (this will be a pass message if no numeric issues),
  #   otherwise need to combine the error messages informatively
  if (!errors) {
    message <- errors_numeric$message
  } else {
    if (!is.null(errors_numeric$missing))
      message <- coordinate_message(input, match, issue = "missing and incorrect")
  }
  
  # return
  list(
    message = message,
    missing = errors_numeric$missing,
    invalid = errors_numeric$invalid,
    unexpected = unexpected,
    errors = errors | errors_numeric$errors,
    type = "coordinate"
  )
  
}
