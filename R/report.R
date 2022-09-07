#' @name generate_report
#' 
#' @title Generate a QA/QC report on a data set for the AAEDB
#' 
#' @export
#' 
#' @importFrom rmarkdown render
#' @importFrom rstudioapi getActiveProject
#' @import dplyr ggplot2
#'
#' @param data a data set to be imported to the AAEDB, provided
#'   as a \code{matrix}, \code{data.frame}, or \code{tibble}
#' @param output name of output file, including path. By default,
#'   will be saved as `template.html` in the top level of the
#'   current working directory
#' 
#' @examples 
#' # to add
generate_report <- function(data, filename = NULL, output = "template.html") {
  
  # set report title
  if (is.null(filename)) {
    filename <- "provided"
  } else {
    filename <- paste0("**", filename, "**")
  }
  
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
  fields_to_check <- list("waterbody", "survey_date", "gear_type", c("x_coordinate", "y_coordinate"))
  
  # but focus on those that exist
  idx <- sapply(fields_to_check, function(x, y) any(x %in% y), y = fields$out_col[fields$matched])
  fields_to_check <- fields_to_check[idx]
  
  # and calculate summaries
  field_summary <- vector("list", length = length(fields_to_check))
  for (i in seq_along(fields_to_check)) {
    
    # summarise the target field
    field_summary[[i]] <- summarise_field(data, validated_fields, field = fields_to_check[[i]])
    
    # add some extra info if it's a survey date
    if (any(fields_to_check[[i]] == "survey_date")) {
      field_summary[[i]]$table <- field_summary[[i]]$table %>%
        mutate(across(
          1,
          list(
            survey_year = ~ year(.x),
            survey_month = ~ month(.x)
          )
        )) %>%
        select(3, 4, 1, 2)
      colnames(field_summary[[i]]$table) <- c("Survey year", "Survey month", "Survey date", "Total records")
    }
    
  }
  
  # check basic size measurements and extract ranges but focus on those that exist
  measurements_to_summarise <- c("total_length", "fork_length", "weight")
  measurements_to_summarise <- measurements_to_summarise[measurements_to_summarise %in% fields$out_col[fields$matched]]
  
  # calculate summaries of measurements (lengths, weights), including some basic data for plotting
  measurement_summary <- vector("list", length = length(measurements_to_summarise))
  for (i in seq_along(measurements_to_summarise))
    measurement_summary[[i]] <- summarise_measurement(data, validated_fields, field = measurements_to_summarise[i])

  # check basic catch info and calculate total catch
  catches_to_summarise <- c("collected", "observed")
  catches_to_summarise <- catches_to_summarise[catches_to_summarise %in% fields$out_col[fields$matched]]
  
  # calculate summaries of catch (collected, observed), including basic data for plotting
  catch_summary <- vector("list", length = length(catches_to_summarise))
  for (i in seq_along(catches_to_summarise))
    catch_summary[[i]] <- summarise_catch(data, validated_fields, field = catches_to_summarise[i])
  
  # check species if provided
  spp_sciname <- spp_common <- NULL
  if ("species" %in% fields$out_col[fields$matched]) {
    
    sp_input <- validated_fields %>% 
      filter(fields == "species") %>% 
      pull(xlsx_fields)
    
    # grab the look-up for gear types
    species_lu <- fetch_vba_species()
    
    # match this to the data set
    spp <- data %>% 
      pull(all_of(sp_input)) %>%
      trimws() %>%
      unique()
    spp <- spp[spp != ""]
    spp <- spp[!is.na(spp)]
    
    # check how many common names and sci names match
    idx <- match(spp, species_lu$scientific_name)
    idy <- match(spp, species_lu$common_name)
    
    # collate into a clean output, check scinames first
    spp_sciname <- NULL
    if (any(!is.na(idx))) {
      spp_sciname <- data.frame(
        spp = spp,
        vba_scientific_name = species_lu$scientific_name[idx],
        vba_common_name = species_lu$common_name[idx],
        did_you_mean = partial_match(spp, species_lu$scientific_name, max_distance = 0.2)
      ) %>%
        mutate(
          vba_scientific_name = ifelse(is.na(vba_scientific_name), "**No match**", vba_scientific_name),
          did_you_mean = ifelse(vba_scientific_name == "**No match**", did_you_mean, "")
        )
    }
    
    # then check common names
    spp_common <- NULL
    if (any(!is.na(idy))) {
      spp_common <- data.frame(
        spp = spp,
        vba_scientific_name = species_lu$scientific_name[idy],
        vba_common_name = species_lu$common_name[idy],
        did_you_mean = partial_match(spp, species_lu$common_name, max_distance = 0.2)
      ) %>%
        mutate(
          vba_common_name = ifelse(is.na(vba_common_name), "**No match**", vba_common_name),
          did_you_mean = ifelse(vba_common_name == "**No match**", did_you_mean, "")
        )
    }
    
    # add catch info
    for (i in seq_along(catch_summary)) {
      if ("species" %in% colnames(catch_summary[[i]]$table)) {
        if (!is.null(spp_sciname)) {
          spp_sciname <- spp_sciname %>% left_join(catch_summary[[i]]$table, by = c("spp" = "species"))
          colnames(spp_sciname)[ncol(spp_sciname)] <- paste0("Total ", catches_to_summarise[i])
        }
        if (!is.null(spp_common)) {
          spp_common <- spp_common %>% left_join(catch_summary[[i]]$table, by = c("spp" = "species"))
          colnames(spp_common)[ncol(spp_common)] <- paste0("Total ", catches_to_summarise[i])
        }
      }
    }
    
  }
  
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

