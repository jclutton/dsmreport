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
#' @param unique_indentifier A character or number. This is the variable name of the
#' unique identifier in your REDCap project. Commonly the unique id is titled record_id.
#'
#'
#' @importFrom REDCapR redcap_read
#' @importFrom janitor clean_names
#' @importFrom rio import
#' @import dplyr
#'
#' @returns A labelled dataframe export of project data
#'
#' @section Notes:
#' 20240412: There needs to be a very strict setup in REDCap. AE and violation need
#' to be repeating instrument. dsmc tool needs to only be in one time point. I need to
#' check the metadata that the project is setup correctly. Need to build later. JC
#' 20240412: Also need to build checks for the unique identifier. JC
#'
#'
get_data <- function(token = NULL, redcap_uri = NULL, which_report = NULL, unique_identifier = NULL) {

  #Use redcap_read to use API to return REDCap data
  df <- REDCapR::redcap_read(redcap_uri = redcap_uri,
                             token = token,
                             raw_or_label = "label")$data

  #Columns of REDCap project
  df_colnames <- colnames(df)

  #Columns we want to be in REDCap project
  cols_of_interest <- data_dictionary %>%
    filter(!field_type %in% c('descriptive','checkbox'))

  #Check for missing columns
  missing_columns <- cols_of_interest %>%
    filter(!variable_field_name %in% df_colnames)

  #Check for checkboxes
  scrn_race_cols <- c('scrn_race___1', 'scrn_race___2', 'scrn_race___4', 'scrn_race___8', 'scrn_race___16', 'scrn_race___32', 'scrn_race___64', 'scrn_race___256')
  checkbox_check <- all(scrn_race_cols %in% df_colnames)

  #Check for all necessary variables in the REDCap project
  if(nrow(missing_columns) > 0){
    stop("Your REDCap is missing key variables. The following variables need to be added to your REDCap project. \n\n ",
         paste0("Variable: ",missing_columns$variable_field_name," in Instrument:",missing_columns$form_name,"\n", collapse = " "),".")
  } else if(!checkbox_check){
    stop("Your REDCap project is missing the variable scrn_race")
  } else {
    df_return <- df %>%
      select(cols_of_interest$variable_field_name, scrn_race_cols)

    return(df_return)
  }

}

