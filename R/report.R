#' @name generate_report
#' 
#' @title Generate a QA/QC report on a data set for the AAEDB
#' 
#' @export
#' 
#' @importFrom rmarkdown render
#' @importFrom rstudioapi getActiveProject
#' @import dplyr
#'
#' @param data a data set to be imported to the AAEDB, provided
#'   as a \code{matrix}, \code{data.frame}, or \code{tibble}
#' @param output name of output file, including path. By default,
#'   will be saved as `template.html` in the top level of the
#'   current working directory
#' 
#' @examples 
#' # to add
generate_report <- function(data, output = "template.html") {
  
  # clean column names
  data <- clean_columns(data)
  
  # extract fields, standard fields, required fields, and validated fields
  fields <- find_matched(data)
  standard_fields <- get_standard(fields)
  required_fields <- check_required(standard_fields)
  validated_fields <- extract_validated(standard_fields)
  
  # format dates in the data set
  data <- format_dates(data, validated_fields)

  # pull out any unmatched fields  
  unmatched_fields <- get_unmatched(fields)
  
  # check for errors in each field
  mga_field <- NULL
  if (any(grepl("mga_zone", validated_fields$fields)))
    mga_field <- validated_fields %>% filter(fields == "mga_zone") %>% pull(xlsx_fields)
  qaqc_outputs <- suppressWarnings(
    apply(validated_fields, 1, validate_data, x = data, mga = mga_field)
  )
      
  # check for duplicates in site descriptions
  duplicates <- check_duplicates(data, validated_fields)
  
  # need to summarise a few fields
  fields_to_check <- list("waterbody", "survey_date", "gear_type", c("x_coordinate", "y_coordinate"), "species")
  
  # but focus on those that exist
  idx <- sapply(fields_to_check, function(x, y) any(x %in% y), y = fields$out_col[fields$matched])
  fields_to_check <- fields_to_check[idx]
  
  # and calculate summaries
  field_summary <- vector("list", length = length(fields_to_check))
  for (i in seq_along(fields_to_check))
    field_summary[[i]] <- summarise_field(data, validated_fields, field = fields_to_check[[i]])
  
  # check basic size measurements and extract ranges
  measurements_to_summarise <- c("total_length", "fork_length", "weight")

  # but focus on those that exist
  measurements_to_summarise <- measurements_to_summarise[measurements_to_summarise %in% fields$out_col[fields$matched]]
  
  # calculate summaries
  measurement_summary <- vector("list", length = length(measurements_to_summarise))
  for (i in seq_along(measurements_to_summarise))
    measurement_summary[[i]] <- summarise_measurements(data, validated_fields, field = measurements_to_summarise[i])
  
  # set rmd path
  rmd_path <- paste0(
    system.file(package = "aae.data"),
    "/rmd/qaqc.Rmd"
  )
  
  # set output path
  output_path <- paste0(rstudioapi::getActiveProject(), "/", output)
  
  # rename objects so the RMD can find them
  required <- required_fields
  unmatched <- unmatched_fields
  qaqc <- qaqc_outputs

  # render report to output file
  rmarkdown::render(
    input = rmd_path,
    output_file = output_path,
    encoding = "UTF-8",
    quiet = TRUE
  )
  
  # return output (currently NULL)
  out <- NULL
  
}

