#### Load language ####
#### Declares language upload loading the package.
.onAttach <- function(libname, pkgname) {

  packageStartupMessage("dsmreport has been loaded")
}

#### Global Variables ####
globalVariables(c("field_type", "variable_field_name"))

#### Get Data ####
#' @name get_data
#'
#' @title Get Data
#'
#' @description This function uses a REDCap API token to create a labelled data frame. This function heavily relies on
#' the work from REDCapR. See \link[REDCapR]{redcap_read}
#'
#' @inheritParams REDCapR::redcap_read
#' @param which_report  A character.
#'   * `All` (the default): creates the full report.
#'   * `recruitment`: only creates the recruitment section of the report
#'   * `safety`: only creates the safety assessment section of the report
#' @param unique_identifier A character or number. This is the variable name of the
#' unique identifier in your REDCap project. Commonly the unique id is titled record_id.
#'
#'
#' @importFrom REDCapR redcap_read
#' @importFrom rio import
#' @import dplyr
#'
#' @returns Dataframes
#'    * `df_return`: All relevant data from the REDCap project. This dataframe only contains columns
#'    necessary for the DSM project. Any other columns are removed
#'    * `meta_data`: A dataframe with the project's data dictionary
#'
#' @section Notes:
#' 20240412: There needs to be a very strict setup in REDCap. AE and violation need
#' to be repeating instrument. dsmc tool needs to only be in one time point. I need to
#' check the metadata that the project is setup correctly. Need to build later. JC
#' 20240412: Also need to build checks for the unique identifier. Another note is that I could
#' rearrage these checks to use the metadata for the project instead of the data. This function
#' uses redcap_metadata_read(). JC
#'
#'
get_data <- function(token = NULL, redcap_uri = NULL, which_report = NULL, unique_identifier = NULL) {

  #Use redcap_read to use API to return REDCap data
  df <- REDCapR::redcap_read(redcap_uri = redcap_uri,
                             token = token,
                             raw_or_label = "label")$data

  meta_data <- REDCapR::redcap_metadata_read(redcap_uri = redcap_uri,
                                             token = token)$data

  #Columns we want to be in REDCap project
  cols_of_interest <- data_dictionary %>%
    filter(!field_type %in% c('descriptive'))

  #Check for missing columns
  missing_columns <- cols_of_interest %>%
    filter(!variable_field_name %in% meta_data$field_name)

  #Check for all necessary variables in the REDCap project
  if(nrow(missing_columns) > 0){
    stop("Your REDCap is missing key variables. The following variables need to be added to your REDCap project. \n\n ",
         paste0("Variable: ",missing_columns$variable_field_name," in Instrument:",missing_columns$form_name,"\n", collapse = " "),".")
  } else {
    df_return <- df %>%
      select(cols_of_interest$variable_field_name)

    return(list(df_return = df_return, meta_data = meta_data))
  }

}

