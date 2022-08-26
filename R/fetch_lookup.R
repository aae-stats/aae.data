#' @name fetch_lookup
#' @title Fetch lookup tables from central Google sheets
#' @description Fetch lookup tables for data fields, standard data fields,
#'   gear types, and VBA species lists from centralised Google sheets.
#' 
#' @examples 
#' # fetch fields
#' fields_lookup <- fetch_fields()
#' 
#' # fetch standard fields
#' standard_fields_lookup <- fetch_standard_fields()
#' 
#' # fetch gear types
#' gear_type_lookup <- fetch_gear_types()
#' 
#' # fetch species list
#' species_lookup <- fetch_vba_species()
NULL

#' @rdname fetch_lookup
#' @export
fetch_fields <- function() {
  id <- "1vVoTNQ3uxFWHzWkUa20zlgiLZS7jh6Vn"
  fetch_google_sheet(id = id)
}

#' @rdname fetch_lookup
#' @export
fetch_standard_fields <- function() {
  id <- "1UOjob2p2IS3bKm55orDzQn_chw4pqwrW"
  fetch_google_sheet(id = id)
}

#' @rdname fetch_lookup
#' @export
fetch_gear_types <- function() {
  id <- "123Bpt944C_OF95J0GwdnvHICwF4_hGrs"
  fetch_google_sheet(id = id)
}

#' @rdname fetch_lookup
#' @export
fetch_vba_species <- function() {
  id <- "11TYRUWSE2Xfdmg_beXt5Hz3fuvX950j5"
  out <- fetch_google_sheet(id = id)
  out$species <- tolower(out$scientific_name)
  out
}

# internal function to download a google sheet specified by unique id
### #' @importFrom googlesheets4 read_sheet
fetch_google_sheet <- function(id) {
  path <- paste0("https://docs.google.com/uc?id=", id, "&export=download")
  read.csv(path)
  # googlesheets4::read_sheet(id)
}
