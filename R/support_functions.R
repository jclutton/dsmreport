#### Load language ####
#### Declares language upload loading the package.
.onAttach <- function(libname, pkgname) {
  
  packageStartupMessage("dsmreport has been loaded")
}

#### Global Variables ####
utils::globalVariables(c("field_type", "variable_field_name",
                         'scrn_date', 'screening_status', 'ieq_scrnfail_rsn','percent','consent_date','rand_date','baseline_status',
                         'reason', 'intervention_status'))

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
    filter(!field_type %in% c('descriptive')) %>%
    filter(variable_field_name != "record_id")
  
  #Check for missing columns
  missing_columns <- cols_of_interest %>%
    filter(!variable_field_name %in% meta_data$field_name)
  
  #Check for all necessary variables in the REDCap project
  if(nrow(missing_columns) > 0){
    stop("Your REDCap is missing key variables. The following variables need to be added to your REDCap project. \n\n ",
         paste0("Variable: ",missing_columns$variable_field_name," in Instrument:",missing_columns$form_name,"\n", collapse = " "),".")
  } else {
    
    df_return <- df %>%
      select(unique_identifier, cols_of_interest$variable_field_name) %>%
      mutate(race = scrn_race,
             ethnicity = case_when(scrn_ethn == "No" ~ "Not Hispanic",
                                   scrn_ethn == "Yes" ~ "Hispanic",
                                   T ~ scrn_ethn),
             sex = scrn_sex,
             age = floor(interval(scrn_dob, consent_date) / years(1)),
             `Protocol Violation` = deviation_type)
    
    return(list(df_return = df_return, meta_data = meta_data))
  }
  
}



#' @name prep_consort
#'
#' @title Prep Consort
#'
#' @description This function transforms data from [get_data()] into consort ready
#' data by sorting participants into groups, i.e. screening, screen-failed, enrolled, etc.
#'
#' @param df A dataframe from [get_data()] with all relevant variables
#' @param meta_data A dataframe with the project REDCap dictionary from [get_data()]
#'
#' @import dplyr
#'
#' @returns Dataframes returned in a list
#'   * `screened`: All screened participants that have a `scrn_date`
#'   * `not_screened`: Participants in the database without a `scrn_date`, presumably to be screened
#'   * `screen_failed`: Participants who screen-fail before consenting, `screening_status` = "Screen-Fail"
#'   * `eligible`: Participants who pass pre-consent screening, `screening_status` = "Eligible"
#'   * `on_hold`: Participants who are on-hold before consent, `screening_status` = On-hold
#'   * `still_screening`: Participants who are presumable still screening because `scrn_date` is not blank but `screening_status` is blank
#'   * `screen_fail_string`: String of pre-consent screenfail reasons and numbers for the consort diagram
#'   * `consented`: Participants with a `consent_date`
#'   * `expected_to_consent`: Participants whose `screening_status` = "Eligible" but `consent_date` is blank
#'   * `randomized`: Participants who have a `rand_date`
#'   * `screen_failed_after_consent`: Participants whose `baseline_status` = Screen Fail
#'   * `in_baseline`: Participants who have a `consent_date` but do not have a `rand_date`
#'   * `screen_fail_baseline_string`: String of baseline screen-fail reasons and numbers for the consort diagram
#'   * `active`: Participants whose `intervention_status` is Actively on intervention or is blank
#'   * `completed`: Participants whose `intervention_status` is Successfully completed
#'   * `withdrawn_testing`: Participants whose `intervention_status` is Withdrawn willing to complete testing
#'   * `withdrawn_no_testing`: Participants whose `intervention_status` is Withdrawn unwilling to complete testing
#'   * `lost`: Participants whose `intervention_status` is Lost to Follow-up
#'   * `group_`: A dataframe is returned for participants in each group. They are titled using the value of the group variable, i.e. group_(value)
#'
#' @section Notes:
#' 20240415: Might want to change the screen fail string to add line breaks every 30 characters.
#'
prep_consort <- function(df = NULL, meta_data = NULL) {
  
  screened <- df %>% filter(!is.na(scrn_date))
  not_screened <- anti_join(df, screened, by = "record_id")
  
  #### Screening Status ####
  #' All screened participants are filtered into statuses
  #A dataframe is kept (remaining) that stores participants
  #Participants are removed from the remaining df as they are given a screening status
  #Any participants left in the remaining df are likely errors and need to be looked into
  screen_failed <- screened %>% filter(screening_status == "Screen-Fail")
  remaining <- anti_join(screened, screen_failed, by = "record_id")
  
  eligible <- remaining %>% filter(screening_status == "Eligible")
  remaining <- anti_join(remaining, eligible, by = "record_id")
  
  on_hold <- remaining %>% filter(screening_status == "On-hold")
  remaining <- anti_join(remaining, on_hold, by = "record_id")
  
  still_screening <- remaining %>% filter(!is.na(scrn_date) & is.na(screening_status))
  remaining <- anti_join(remaining, still_screening, by = "record_id")
  
  #### Screen Fail Reasons ####
  screen_fail_reasons <- screen_failed %>%
    group_by(ieq_scrnfail_rsn) %>%
    summarise(n = n(),
              percent = round(n/nrow(screen_failed)*100, digits = 0)) %>%
    mutate(string = paste0("- ",ieq_scrnfail_rsn," (n = ",n,")")) %>%
    mutate(length = stringr::str_length(string)) %>%
    mutate(string = case_when(stringr::str_length(string) > 30 ~ stringr::str_replace(string, "[:blank:](?=[:punct:]n)", "\n"),
                              T ~ string))
  #stringi::stri_escape_unicode("•")
  
  
  screen_fail_string <- paste0(screen_fail_reasons$string, collapse = "\n")
  #
  #   phone_screen_mistakes <- screen_fail_reason %>%
  #     filter(is.na(ieq_scrnfail_rsn))
  
  
  #### Second part of consort Diagram ####
  consented <- eligible %>% filter(!is.na(consent_date))
  remaining_2 <- anti_join(eligible, consented, by = "record_id")
  
  expected_to_consent <- remaining_2
  
  ##### Third part of consort diagram ####
  randomized <- consented %>% filter(!is.na(rand_date))
  remaining_3 <- anti_join(consented, randomized, by = "record_id")
  
  screen_failed_after_consent <- remaining_3 %>% filter(baseline_status == "Screen Fail")
  remaining_3 <- anti_join(remaining_3, screen_failed_after_consent, by = "record_id")
  
  in_baseline <- remaining_3 %>% filter(!is.na(consent_date) & is.na(rand_date))
  remaining_3 <- anti_join(remaining_3, in_baseline, by = "record_id")
  
  
  #### Post-consent screen-fail Reasons ####
  screen_fail_baseline_reason <- screen_failed_after_consent %>%
    mutate(reason = case_when(!is.na(baseline_scrnfail_ie) ~ baseline_scrnfail_ie,
                              !is.na(baseline_scrnfail_nocont) ~ baseline_scrnfail_nocont)) %>%
    group_by(reason) %>%
    summarise(n = n(),
              percent = round(n/nrow(screen_failed_after_consent)*100, digits = 0)) %>%
    mutate(string = paste0("- ",reason," (n = ",n,")")) %>%
    mutate(length = stringr::str_length(string)) %>%
    mutate(string = case_when(stringr::str_length(string) > 30 ~ stringr::str_replace(string, "[:blank:](?=[:punct:]n)", "\n"),
                              T ~ string))
  
  screen_fail_baseline_string <- paste0(screen_fail_baseline_reason$string, collapse = "\n")
  
  screen_fail_baseline_mistakes <- screen_fail_baseline_reason %>%
    filter(is.na(reason))
  
  
  #### Fourth part of consort diagram  ####
  active <- randomized %>% filter(intervention_status == "Actively on intervention" | is.na(intervention_status))
  remaining_4 <- anti_join(randomized, active, by = "record_id")
  
  completed <- remaining_4 %>% filter(intervention_status == "Successfully completed")
  remaining_4 <- anti_join(remaining_4, completed, by = "record_id")
  
  withdrawn_testing <- remaining_4 %>% filter(intervention_status == "Withdrawn willing to complete testing")
  remaining_4 <- anti_join(remaining_4, withdrawn_testing, by = "record_id")
  
  withdrawn_no_testing <- remaining_4 %>% filter(intervention_status == "Withdrawn unwilling to complete testing")
  remaining_4 <- anti_join(remaining_4, withdrawn_no_testing, by = "record_id")
  
  lost <- remaining_4 %>% filter(intervention_status == "Lost to Follow-up")
  remaining_4 <- anti_join(remaining_4, lost, by = "record_id")
  
  
  #### Return Dataframes ####
  return(list(screened = screened,
              not_screened = not_screened,
              screen_failed = screen_failed,
              eligible = eligible,
              on_hold = on_hold,
              still_screening = still_screening,
              screen_fail_string = screen_fail_string,
              consented = consented,
              expected_to_consent = expected_to_consent,
              randomized = randomized,
              screen_failed_after_consent = screen_failed_after_consent,
              in_baseline = in_baseline,
              screen_fail_baseline_string = screen_fail_baseline_string,
              active = active,
              completed = completed,
              withdrawn_testing = withdrawn_testing,
              withdrawn_no_testing = withdrawn_no_testing,
              lost = lost,
              screen_fail_reasons = screen_fail_reasons))
}

#' @name screening_consort
#'
#' @title Create Screening Consort Image
#'
#' @description This function uses the dataframes created in [prep_consort()] to create a
#' screening consort diagram. THe screening consort image contains number screened, screen-failed
#' still screening, and eligible.
#'
#' @param consort_data A list of dataframes from [prep_consort()]
#'   * `screened`: All screened participants that have a `scrn_date`
#'   * `not_screened`: Participants in the database without a `scrn_date`, presumably to be screened
#'   * `screen_failed`: Participants who screen-fail before consenting, `screening_status` = "Screen-Fail"
#'   * `eligible`: Participants who pass pre-consent screening, `screening_status` = "Eligible"
#'   * `on_hold`: Participants who are on-hold before consent, `screening_status` = On-hold
#'   * `still_screening`: Participants who are presumable still screening because `scrn_date` is not blank but `screening_status` is blank
#'   * `screen_fail_string`: String of pre-consent screenfail reasons and numbers for the consort diagram
#' @param filepath A file path to save the screening consort diagram image
#'
#'  @returns plot: a ggplot object
#'
#'  @import dplyr
#'  @import ggplot2
#'
#'
screening_consort <- function(consort_data = NULL, filepath = NULL) {
  #### Screening Consort Diagram ####
  #global settings
  num_screenfail_rsns <- stringr::str_count(consort_data$screen_fail_string, pattern = "\\n") + 1
  
  if(num_screenfail_rsns < 10){
    #### ggplot settings ####
    font_size <- 4
    font_size_smaller <- 2.5
    line_size <- .75
    
    grid_size <- data.frame(x = 100, y = 100)
    
    screening_consort <- data.frame(name = c('screened_language',
                                             'eligible_language',
                                             'still_screening_language',
                                             'screen_fail_language'
    ),
    language = c(paste0("Screened (n = ",nrow(consort_data$screened),")"),
                 paste0("Eligible for Consent (n = ",nrow(consort_data$eligible),")"),
                 paste0("Still Screening (n = ",nrow(consort_data$on_hold) + nrow(consort_data$still_screening),")"),
                 paste0("Screen Failed (n = ",nrow(consort_data$screen_failed),")\n", consort_data$screen_fail_string)
    ),
    x = c(grid_size$x/2,
          grid_size$x/2,
          20,
          grid_size$x-15),
    y = c(grid_size$y - 0,
          grid_size$y - (20 + num_screenfail_rsns*5 + 20),
          grid_size$y - (10 + num_screenfail_rsns*5),
          grid_size$y - (10 + num_screenfail_rsns*5)
    )) %>%
      mutate(text_width = stringr::str_count(language) * 1.9) %>%
      mutate(left_box = x - text_width/2,
             right_box = x + text_width/2,
             up_box = y + 5,
             down_box = y - 5)
    
    vertical_arrow_top <- screening_consort$y[which(screening_consort$name == "screened_language")] - 3
    vertical_arrow_bottom <- screening_consort$y[which(screening_consort$name == "eligible_language")] + 5
    vertical_arrow_x <- screening_consort$x[which(screening_consort$name == "screened_language")]
    
    h_arrow_y <- screening_consort$y[which(screening_consort$name == "still_screening_language")]
    h_arrow_start <- screening_consort$x[2]
    h_arrow_d <- 10
    
    data <- tibble(x= 1:grid_size$x, y= 1:grid_size$y)
    
    
    plot <- ggplot2::ggplot(screening_consort, ggplot2::aes(x,
                                                            y,
                                                            label = language)) +
      ggplot2::geom_label(fontface = "bold", size = font_size, label.size = line_size) +
      ggplot2::scale_x_continuous(limits = c(-10, grid_size$x + 10)) +
      ggplot2::scale_y_continuous(limits = c(-10, grid_size$y + 10)) +
      ggplot2::geom_segment(
        x=vertical_arrow_x, xend=vertical_arrow_x, y=vertical_arrow_top, yend=vertical_arrow_bottom,
        size=line_size, linejoin = "mitre", lineend = "butt",
        arrow = ggplot2::arrow(length = ggplot2::unit(1, "mm"), type= "closed")) +
      ggplot2::geom_segment(
        x=h_arrow_start, xend=h_arrow_start - h_arrow_d, y=h_arrow_y, yend=h_arrow_y,
        size=line_size, linejoin = "mitre", lineend = "butt",
        arrow = ggplot2::arrow(length = ggplot2::unit(1, "mm"), type= "closed")) +
      ggplot2::geom_segment(
        x=h_arrow_start, xend=h_arrow_start + h_arrow_d, y=h_arrow_y, yend=h_arrow_y,
        size=line_size, linejoin = "mitre", lineend = "butt",
        arrow = ggplot2::arrow(length = ggplot2::unit(1, "mm"), type= "closed")) +
      ggplot2::annotate("label", x = 0, y = grid_size$y,
                        label = "Screening", fill = "#9bc0fc", label.size = NA) +
      ggplot2::theme_void()
  }
  
  ggplot2::ggsave(plot, filename = 'screening_consort.jpg', path = filepath,
                  width = 7, height = 7, units = "in",
                  device = "jpg")
  
  return(plot)
  
  
}

#' @name closed_consort
#'
#' @title Create Closed Consort Image
#'
#' @description This function uses the dataframes created in [prep_consort()] to create a
#' closed consort diagram. The consort diagram is not a standard consort diagram, as it is
#' designed for ongoing trials; however, it conveys the same information.
#'
#' @param consort_data A list of dataframes from [prep_consort()]
#'   * `screened`: All screened participants that have a `scrn_date`
#'   * `not_screened`: Participants in the database without a `scrn_date`, presumably to be screened
#'   * `screen_failed`: Participants who screen-fail before consenting, `screening_status` = "Screen-Fail"
#'   * `eligible`: Participants who pass pre-consent screening, `screening_status` = "Eligible"
#'   * `on_hold`: Participants who are on-hold before consent, `screening_status` = On-hold
#'   * `still_screening`: Participants who are presumable still screening because `scrn_date` is not blank but `screening_status` is blank
#'   * `screen_fail_string`: String of pre-consent screenfail reasons and numbers for the consort diagram
#'   * `consented`: Participants with a `consent_date`
#'   * `expected_to_consent`: Participants whose `screening_status` = "Eligible" but `consent_date` is blank
#'   * `randomized`: Participants who have a `rand_date`
#'   * `screen_failed_after_consent`: Participants whose `baseline_status` = Screen Fail
#'   * `in_baseline`: Participants who have a `consent_date` but do not have a `rand_date`
#'   * `screen_fail_baseline_string`: String of baseline screen-fail reasons and numbers for the consort diagram
#'   * `active`: Participants whose `intervention_status` is Actively on intervention or is blank
#'   * `completed`: Participants whose `intervention_status` is Successfully completed
#'   * `withdrawn_testing`: Participants whose `intervention_status` is Withdrawn willing to complete testing
#'   * `withdrawn_no_testing`: Participants whose `intervention_status` is Withdrawn unwilling to complete testing
#'   * `lost`: Participants whose `intervention_status` is Lost to Follow-up
#'  @inheritParams prep_consort
#'  @param filepath The path to a folder to save a svg image of the consort diagram
#'
#'
#'  @returns Images as .png files
#'  * Post-consent diagram
#'
#'  @importFrom REDCapR checkbox_choices
#'  @import dplyr
#'  @import ggplot2
#'
#'  @section Notes:
#'  20240418: Idea for later. Have constants with the subtract or additions that change depending on the size of the grid so that how much I'm subtracting
#'  is relative to image. Also, group 1 and 2 start central. Then group 3 and four move to the sides. JC
#'
#'
#'
closed_consort <- function(consort_data = NULL, meta_data = NULL, filepath = NULL) {
  
  #### Screening Consort Diagram ####
  #global settings
  num_screenfail_rsns <- stringr::str_count(consort_data$screen_fail_baseline_string, pattern = "\\n") + 1
  
  # # Use checkbox_choices from REDCapR to return IDs and labels of groups
  groups <- REDCapR::checkbox_choices(meta_data$select_choices_or_calculations[which(meta_data$field_name == "group")]) %>%
    mutate(names = paste0("group_",id))
  
  for(i in 1:nrow(groups)){
    
    temp_df <- consort_data$randomized %>%
      filter(group == groups$label[i])
    
    groups$group_num[i] <- i
    
    groups$group_language[i] <- paste0(groups$label[i],"\n (n = ",nrow(temp_df),")")
    
    groups$status_language[i] <- paste0("Active (n = ",sum(temp_df$intervention_status == "Actively on intervention" | is.na(temp_df$intervention_status)),")\n",
                                        "Lost to follow-up (n = ",sum(temp_df$intervention_status == "Lost to Follow-up", na.rm = T),")\n",
                                        "Withdrawn \n Unwilling to test (n = ",sum(temp_df$intervention_status == "Withdrawn unwilling to complete testing", na.rm = T),")\n",
                                        "Willing to test (n = ",sum(temp_df$intervention_status == "Withdrawn willing to complete testing", na.rm = T),")\n")
    
    groups$completed_language[i] <- paste0("Completed\n (n = ",sum(temp_df$intervention_status == "Successfully completed", na.rm = T),")")
    
  }
  
  group_df_small <- tidyr::pivot_longer(groups, cols = contains("language"), values_to = "language") %>%
    filter(name == "status_language")
  
  group_df <- tidyr::pivot_longer(groups, cols = contains("language"), values_to = "language") %>%
    filter(name != "status_language")
  
  
  
  if(num_screenfail_rsns < 10){
    #### ggplot settings ####
    font_size <- 3.5
    font_size_smaller <- 2.5
    line_size <- .75
    
    grid_size <- data.frame(x = 200, y = 200)
    
    consort_start <- data.frame(name = c('eligible_language',
                                         'in_process_language',
                                         'consented_language',
                                         'in_baseline_language',
                                         'screen_failed_baseline_language',
                                         'randomized_language'
    ),
    language = c(paste0("Eligible for Consent (n = ",nrow(consort_data$eligible),")"),
                 paste0("Expected to Consent \n(n = ",nrow(consort_data$expected_to_consent),")"),
                 paste0("Consented (n = ",nrow(consort_data$consented),")"),
                 paste0("In Baseline (n = ",nrow(consort_data$in_baseline),")"),
                 paste0("Screen Failed (n = ",nrow(consort_data$screen_failed_after_consent),")\n",consort_data$screen_fail_baseline_string),
                 paste0("Randomized (n = ",nrow(consort_data$randomized),")")
                 
    ),
    x = c(grid_size$x/2,
          grid_size$x/2 - 60,
          grid_size$x/2,
          grid_size$x/2 - 60,
          grid_size$x/2 + 70,
          grid_size$x/2),
    y = c(grid_size$y - 0,
          grid_size$y - 20,
          grid_size$y - 40,
          grid_size$y - (50 + num_screenfail_rsns*10),
          grid_size$y - (50 + num_screenfail_rsns*10),
          grid_size$y - (50 + num_screenfail_rsns*10 + 20)
    ))
    
    consort <- bind_rows(consort_start, group_df) %>%
      mutate(y = case_when(name == "group_language" ~ consort_start$y[which(name == "randomized_language")] - 20,
                           #name == "status_language" ~ consort_start$y[which(name == "randomized_language")] - 60,
                           name == "completed_language" ~ consort_start$y[which(name == "randomized_language")] - 80,
                           T~ y),
             x = case_when(name %in% c('group_language','completed_language') & group_num == 1 ~ grid_size$x/2 - 25,
                           group_num == 1 ~ grid_size$x/2-10,
                           name %in% c('group_language','completed_language') & group_num == 2 ~ grid_size$x/2 + 25,
                           group_num == 2 ~ grid_size$x/2 + 40,
                           T ~ x)) %>%
      mutate(fontsize = case_when(name == 'status_language' ~ font_size_smaller,
                                  T ~ font_size))
    
    #### HIDE ####
    vert_arrow1 <- data.frame(x = consort$x[which(consort$name == "eligible_language")],
                              ystart = consort$y[which(consort$name == "eligible_language")] - 3,
                              yend = consort$y[which(consort$name == "consented_language")] + 7)
    
    h_arrow1 <- data.frame(y = consort$y[which(consort$name == "in_process_language")],
                           xstart = consort$x[which(consort$name == "eligible_language")] ,
                           xend = consort$x[which(consort$name == "in_process_language")] + 40)
    
    v_arrow2 <- data.frame(x = consort$x[which(consort$name == "consented_language")],
                           ystart = consort$y[which(consort$name == "consented_language")] - 3,
                           yend = consort$y[which(consort$name == "randomized_language")] + 7)
    
    h_arrow2_l <- data.frame(y = consort$y[which(consort$name == "in_baseline_language")],
                             xstart = consort$x[which(consort$name == "consented_language")] ,
                             xend = consort$x[which(consort$name == "in_baseline_language")] + 40)
    
    h_arrow2_r <- data.frame(y = consort$y[which(consort$name == "screen_failed_baseline_language")],
                             xstart = consort$x[which(consort$name == "consented_language")] ,
                             xend = consort$x[which(consort$name == "screen_failed_baseline_language")] - 50)
    
    hline3 <- data.frame(y = consort$y[which(consort$name == "randomized_language")] - 7,
                         xstart = consort$x[which(consort$name == "group_language" & consort$names == "group_1")]  ,
                         xend = consort$x[which(consort$name == "group_language" & consort$names == "group_2")])
    
    vline_g1 <- data.frame(x = hline3$xstart,
                           ystart = hline3$y,
                           yend = consort$y[which(consort$name == "group_language" & consort$names == "group_1")] + 7)
    
    vline_g2 <- data.frame(x = hline3$xend,
                           ystart = hline3$y,
                           yend = consort$y[which(consort$name == "group_language" & consort$names == "group_2")] + 7)
    
    v_arrow_g1 <- data.frame(x = consort$x[which(consort$name == "group_language" & consort$names == "group_1")],
                             ystart = consort$y[which(consort$name == "group_language" & consort$names == "group_1")] - 7,
                             yend = consort$y[which(consort$name == "completed_language" & consort$names == "group_1")] + 10)
    
    v_arrow_g2 <- data.frame(x = consort$x[which(consort$name == "group_language" & consort$names == "group_2")],
                             ystart = consort$y[which(consort$name == "group_language" & consort$names == "group_2")] - 7 ,
                             yend = consort$y[which(consort$name == "completed_language" & consort$names == "group_2")] + 10)
    
    group_df_small_consort <- group_df_small %>%
      mutate(x = case_when(names == "group_1" ~ v_arrow_g1$x + 25,
                           names == "group_2" ~ v_arrow_g2$x + 25),
             y = (v_arrow_g1$ystart - v_arrow_g2$yend)/2 + v_arrow_g2$yend)
    
    
    
    data <- tibble(x= 1:grid_size$x, y= 1:grid_size$y)
    
    
    plot <- ggplot2::ggplot(consort, ggplot2::aes(x,
                                                  y,
                                                  label = language)) +
      ggplot2::geom_label(fontface = "bold", size = font_size, label.size = line_size) +
      ggplot2::scale_x_continuous(limits = c(-10, grid_size$x + 10)) +
      ggplot2::scale_y_continuous(limits = c(-10, grid_size$y + 10)) +
      ggplot2::geom_segment(
        x=vert_arrow1$x, xend=vert_arrow1$x, y=vert_arrow1$ystart, yend=vert_arrow1$yend,
        size=line_size, linejoin = "mitre", lineend = "butt",
        arrow = ggplot2::arrow(length = ggplot2::unit(1, "mm"), type= "closed")) +
      ggplot2::geom_segment(
        x=h_arrow1$xstart, xend=h_arrow1$xend, y=h_arrow1$y, yend=h_arrow1$y,
        size=line_size, linejoin = "mitre", lineend = "butt",
        arrow = ggplot2::arrow(length = ggplot2::unit(1, "mm"), type= "closed")) +
      ggplot2::geom_segment(
        x=v_arrow2$x, xend=v_arrow2$x, y=v_arrow2$ystart, yend=v_arrow2$yend,
        size=line_size, linejoin = "mitre", lineend = "butt",
        arrow = ggplot2::arrow(length = ggplot2::unit(1, "mm"), type= "closed")) +
      ggplot2::geom_segment(
        x=h_arrow2_l$xstart, xend=h_arrow2_l$xend, y=h_arrow2_l$y, yend=h_arrow2_l$y,
        size=line_size, linejoin = "mitre", lineend = "butt",
        arrow = ggplot2::arrow(length = ggplot2::unit(1, "mm"), type= "closed")) +
      ggplot2::geom_segment(
        x=h_arrow2_r$xstart, xend=h_arrow2_r$xend, y=h_arrow2_r$y, yend=h_arrow2_r$y,
        size=line_size, linejoin = "mitre", lineend = "butt",
        arrow = ggplot2::arrow(length = ggplot2::unit(1, "mm"), type= "closed")) +
      ggplot2::geom_segment(
        x=v_arrow_g1$x, xend=v_arrow_g1$x, y=v_arrow_g1$ystart, yend=v_arrow_g1$yend,
        size=line_size, linejoin = "mitre", lineend = "butt",
        arrow = ggplot2::arrow(length = ggplot2::unit(1, "mm"), type= "closed")) +
      ggplot2::geom_segment(
        x=v_arrow_g2$x, xend=v_arrow_g2$x, y=v_arrow_g2$ystart, yend=v_arrow_g2$yend,
        size=line_size, linejoin = "mitre", lineend = "butt",
        arrow = ggplot2::arrow(length = ggplot2::unit(1, "mm"), type= "closed")) +
      ggplot2::geom_segment(
        x=hline3$xstart, xend=hline3$xend, y=hline3$y, yend=hline3$y,
        size=line_size, linejoin = "mitre", lineend = "butt") +
      ggplot2::geom_segment(
        x=vline_g1$x, xend=vline_g1$x, y=vline_g1$ystart, yend=vline_g1$yend,
        size=line_size, linejoin = "mitre", lineend = "butt") +
      ggplot2::geom_segment(
        x=vline_g2$x, xend=vline_g2$x, y=vline_g2$ystart, yend=vline_g2$yend,
        size=line_size, linejoin = "mitre", lineend = "butt") +
      ggplot2::geom_label(data = group_df_small_consort,
                          ggplot2::aes(x,
                                       y,
                                       label = language),
                          fontface = "bold", size = font_size_smaller, label.size = line_size) +
      
      ggplot2::annotate("label", x = 10, y = grid_size$y,
                        label = "Eligible", fill = "#9bc0fc", label.size = NA) +
      ggplot2::annotate("label", x = 10, y = v_arrow2$ystart,
                        label = "Enrollment", fill = "#9bc0fc", label.size = NA) +
      ggplot2::annotate("label", x = 10, y = consort$y[which(consort$name == "randomized_language")] ,
                        label = "Allocation", fill = "#9bc0fc", label.size = NA) +
      ggplot2::annotate("label", x = 10, y = v_arrow_g1$yend ,
                        label = "Follow-up", fill = "#9bc0fc", label.size = NA) +
      ggplot2::theme_void()
  }
  
  plot
  
  ggplot2::ggsave(plot, filename = 'enrollment_consort_closed.jpg', path = filepath,
                  height = 8.7,
                  width = 14.2,
                  units = "in",
                  device = "jpg")
  
  return(plot)
  
}

#' @name open_consort
#'
#' @title Create Open Consort Image
#'
#' @description This function uses the dataframes created in [prep_consort()] to create an
#' open consort diagram. The consort diagram is not a standard consort diagram, as it is
#' designed for ongoing trials; however, it conveys the same information.
#'
#' @param consort_data A list of dataframes from [prep_consort()]
#'   * `screened`: All screened participants that have a `scrn_date`
#'   * `not_screened`: Participants in the database without a `scrn_date`, presumably to be screened
#'   * `screen_failed`: Participants who screen-fail before consenting, `screening_status` = "Screen-Fail"
#'   * `eligible`: Participants who pass pre-consent screening, `screening_status` = "Eligible"
#'   * `on_hold`: Participants who are on-hold before consent, `screening_status` = On-hold
#'   * `still_screening`: Participants who are presumable still screening because `scrn_date` is not blank but `screening_status` is blank
#'   * `screen_fail_string`: String of pre-consent screenfail reasons and numbers for the consort diagram
#'   * `consented`: Participants with a `consent_date`
#'   * `expected_to_consent`: Participants whose `screening_status` = "Eligible" but `consent_date` is blank
#'   * `randomized`: Participants who have a `rand_date`
#'   * `screen_failed_after_consent`: Participants whose `baseline_status` = Screen Fail
#'   * `in_baseline`: Participants who have a `consent_date` but do not have a `rand_date`
#'   * `screen_fail_baseline_string`: String of baseline screen-fail reasons and numbers for the consort diagram
#'   * `active`: Participants whose `intervention_status` is Actively on intervention or is blank
#'   * `completed`: Participants whose `intervention_status` is Successfully completed
#'   * `withdrawn_testing`: Participants whose `intervention_status` is Withdrawn willing to complete testing
#'   * `withdrawn_no_testing`: Participants whose `intervention_status` is Withdrawn unwilling to complete testing
#'   * `lost`: Participants whose `intervention_status` is Lost to Follow-up
#'  @inheritParams prep_consort
#'  @param filepath The path to a folder to save a svg image of the consort diagram
#'
#'
#'  @returns Images as .png files
#'  * Post-consent diagram
#'
#'  @importFrom REDCapR checkbox_choices
#'  @import dplyr
#'  @import ggplot2
#'
#'  @section Notes:
#'  20240418: Idea for later. Have constants with the subtract or additions that change depending on the size of the grid so that how much I'm subtracting
#'  is relative to image. Also, group 1 and 2 start central. Then group 3 and four move to the sides. JC
#'
#'
#'
open_consort <- function(consort_data = NULL, meta_data = NULL, filepath = NULL) {
  
  #### Screening Consort Diagram ####
  #global settings
  num_screenfail_rsns <- stringr::str_count(consort_data$screen_fail_baseline_string, pattern = "\\n") + 1
  
  
  if(num_screenfail_rsns < 10){
    #### ggplot settings ####
    font_size <- 3.5
    font_size_smaller <- 2.5
    line_size <- .75
    
    grid_size <- data.frame(x = 200, y = 200)
    
    consort <- data.frame(name = c('eligible_language',
                                   'in_process_language',
                                   'consented_language',
                                   'in_baseline_language',
                                   'screen_failed_baseline_language',
                                   'randomized_language',
                                   'active_language',
                                   'completed_language',
                                   'withdrawn_willing_language',
                                   'withdrawn_unwilling_language',
                                   'lost_language'
    ),
    language = c(paste0("Eligible for Consent (n = ",nrow(consort_data$eligible),")"),
                 paste0("Expected to Consent \n(n = ",nrow(consort_data$expected_to_consent),")"),
                 paste0("Consented (n = ",nrow(consort_data$consented),")"),
                 paste0("In Baseline\n(n = ",nrow(consort_data$in_baseline),")"),
                 paste0("Screen Failed (n = ",nrow(consort_data$screen_failed_after_consent),")\n",consort_data$screen_fail_baseline_string),
                 paste0("Randomized (n = ",nrow(consort_data$randomized),")"),
                 paste0("Active on Intervention\n(n = ",nrow(consort_data$active),")"),
                 paste0("Sucessfully Completed\n(n = ",nrow(consort_data$completed),")"),
                 paste0("Withdrawn\nWilling to\nComplete Testing\n(n = ",nrow(consort_data$withdrawn_testing),")"),
                 paste0("Withdrawn\nUnwilling to\nComplete Testing\n(n = ",nrow(consort_data$withdrawn_no_testing),")"),
                 paste0("Lost to Follow-up\n(n = ",nrow(consort_data$lost),")")
                 
    ),
    x = c(grid_size$x/2,
          grid_size$x/2 - 50,
          grid_size$x/2,
          grid_size$x/2 - 50,
          grid_size$x/2 + 50,
          grid_size$x/2,
          grid_size$x/2-60,
          grid_size$x/2-30,
          grid_size$x/2,
          grid_size$x/2+30,
          grid_size$x/2+60),
    y = c(grid_size$y - 0,
          grid_size$y - 20,
          grid_size$y - 40,
          grid_size$y - (50 + num_screenfail_rsns*10),
          grid_size$y - (50 + num_screenfail_rsns*10),
          grid_size$y - (50 + num_screenfail_rsns*10 + 20),
          grid_size$y - (50 + num_screenfail_rsns*10 + 20 + 50),
          grid_size$y - (50 + num_screenfail_rsns*10 + 20 + 50),
          grid_size$y - (50 + num_screenfail_rsns*10 + 20 + 50),
          grid_size$y - (50 + num_screenfail_rsns*10 + 20 + 50),
          grid_size$y - (50 + num_screenfail_rsns*10 + 20 + 50)
    ))
    
    
    #### HIDE ####
    vert_arrow1 <- data.frame(x = consort$x[which(consort$name == "eligible_language")],
                              ystart = consort$y[which(consort$name == "eligible_language")] - 3,
                              yend = consort$y[which(consort$name == "consented_language")] + 7)
    
    h_arrow1 <- data.frame(y = consort$y[which(consort$name == "in_process_language")],
                           xstart = consort$x[which(consort$name == "eligible_language")] ,
                           xend = consort$x[which(consort$name == "in_process_language")] + 30)
    
    v_arrow2 <- data.frame(x = consort$x[which(consort$name == "consented_language")],
                           ystart = consort$y[which(consort$name == "consented_language")] - 3,
                           yend = consort$y[which(consort$name == "randomized_language")] + 7)
    
    h_arrow2_l <- data.frame(y = consort$y[which(consort$name == "in_baseline_language")],
                             xstart = consort$x[which(consort$name == "consented_language")] ,
                             xend = consort$x[which(consort$name == "in_baseline_language")] + 30)
    
    h_arrow2_r <- data.frame(y = consort$y[which(consort$name == "screen_failed_baseline_language")],
                             xstart = consort$x[which(consort$name == "consented_language")] ,
                             xend = consort$x[which(consort$name == "screen_failed_baseline_language")] - 30)
    
    hline3 <- data.frame(y = consort$y[which(consort$name == "randomized_language")] - 7,
                         xstart = consort$x[which(consort$name == "active_language")]  ,
                         xend = consort$x[which(consort$name == "lost_language")])
    
    v_arrow_active <- data.frame(x = consort$x[which(consort$name == "active_language")],
                                 ystart = consort$y[which(consort$name == "randomized_language")] - 7,
                                 yend = consort$y[which(consort$name == "active_language")] + 15)
    
    v_arrow_completed <- data.frame(x = consort$x[which(consort$name == "completed_language")],
                                    ystart = consort$y[which(consort$name == "randomized_language")] - 7,
                                    yend = consort$y[which(consort$name == "active_language")] + 15)
    
    v_arrow_willing <- data.frame(x = consort$x[which(consort$name == "withdrawn_willing_language")],
                                  ystart = consort$y[which(consort$name == "randomized_language")] - 7,
                                  yend = consort$y[which(consort$name == "active_language")] + 15)
    
    v_arrow_unwilling <- data.frame(x = consort$x[which(consort$name == "withdrawn_unwilling_language")],
                                    ystart = consort$y[which(consort$name == "randomized_language")] - 7,
                                    yend = consort$y[which(consort$name == "active_language")] + 15)
    
    v_arrow_lost <- data.frame(x = consort$x[which(consort$name == "lost_language")],
                               ystart = consort$y[which(consort$name == "randomized_language")] - 7,
                               yend = consort$y[which(consort$name == "active_language")] + 15)
    
    
    
    
    data <- tibble(x= 1:grid_size$x, y= 1:grid_size$y)
    
    
    plot <- ggplot2::ggplot(consort, ggplot2::aes(x,
                                                  y,
                                                  label = language)) +
      ggplot2::geom_label(fontface = "bold", size = font_size, label.size = line_size) +
      ggplot2::scale_x_continuous(limits = c(-10, grid_size$x + 10)) +
      ggplot2::scale_y_continuous(limits = c(-10, grid_size$y + 10)) +
      ggplot2::geom_segment(
        x=vert_arrow1$x, xend=vert_arrow1$x, y=vert_arrow1$ystart, yend=vert_arrow1$yend,
        size=line_size, linejoin = "mitre", lineend = "butt",
        arrow = ggplot2::arrow(length = ggplot2::unit(1, "mm"), type= "closed")) +
      ggplot2::geom_segment(
        x=h_arrow1$xstart, xend=h_arrow1$xend, y=h_arrow1$y, yend=h_arrow1$y,
        size=line_size, linejoin = "mitre", lineend = "butt",
        arrow = ggplot2::arrow(length = ggplot2::unit(1, "mm"), type= "closed")) +
      ggplot2::geom_segment(
        x=v_arrow2$x, xend=v_arrow2$x, y=v_arrow2$ystart, yend=v_arrow2$yend,
        size=line_size, linejoin = "mitre", lineend = "butt",
        arrow = ggplot2::arrow(length = ggplot2::unit(1, "mm"), type= "closed")) +
      ggplot2::geom_segment(
        x=h_arrow2_l$xstart, xend=h_arrow2_l$xend, y=h_arrow2_l$y, yend=h_arrow2_l$y,
        size=line_size, linejoin = "mitre", lineend = "butt",
        arrow = ggplot2::arrow(length = ggplot2::unit(1, "mm"), type= "closed")) +
      ggplot2::geom_segment(
        x=h_arrow2_r$xstart, xend=h_arrow2_r$xend, y=h_arrow2_r$y, yend=h_arrow2_r$y,
        size=line_size, linejoin = "mitre", lineend = "butt",
        arrow = ggplot2::arrow(length = ggplot2::unit(1, "mm"), type= "closed")) +
      ggplot2::geom_segment(
        x=hline3$xstart, xend=hline3$xend, y=hline3$y, yend=hline3$y,
        size=line_size, linejoin = "mitre", lineend = "butt") +
      ggplot2::geom_segment(
        x=v_arrow_active$x, xend=v_arrow_active$x, y=v_arrow_active$ystart, yend=v_arrow_active$yend,
        size=line_size, linejoin = "mitre", lineend = "butt",
        arrow = ggplot2::arrow(length = ggplot2::unit(1, "mm"), type= "closed")) +
      ggplot2::geom_segment(
        x=v_arrow_completed$x, xend=v_arrow_completed$x, y=v_arrow_completed$ystart, yend=v_arrow_completed$yend,
        size=line_size, linejoin = "mitre", lineend = "butt",
        arrow = ggplot2::arrow(length = ggplot2::unit(1, "mm"), type= "closed")) +
      ggplot2::geom_segment(
        x=v_arrow_willing$x, xend=v_arrow_willing$x, y=v_arrow_willing$ystart, yend=v_arrow_willing$yend,
        size=line_size, linejoin = "mitre", lineend = "butt",
        arrow = ggplot2::arrow(length = ggplot2::unit(1, "mm"), type= "closed")) +
      ggplot2::geom_segment(
        x=v_arrow_unwilling$x, xend=v_arrow_unwilling$x, y=v_arrow_unwilling$ystart, yend=v_arrow_unwilling$yend,
        size=line_size, linejoin = "mitre", lineend = "butt",
        arrow = ggplot2::arrow(length = ggplot2::unit(1, "mm"), type= "closed")) +
      ggplot2::geom_segment(
        x=v_arrow_lost$x, xend=v_arrow_lost$x, y=v_arrow_lost$ystart, yend=v_arrow_lost$yend,
        size=line_size, linejoin = "mitre", lineend = "butt",
        arrow = ggplot2::arrow(length = ggplot2::unit(1, "mm"), type= "closed")) +
      
      ggplot2::annotate("label", x = 10, y = grid_size$y,
                        label = "Eligible", fill = "#9bc0fc", label.size = NA) +
      ggplot2::annotate("label", x = 10, y = v_arrow2$ystart,
                        label = "Enrollment", fill = "#9bc0fc", label.size = NA) +
      ggplot2::annotate("label", x = 10, y = consort$y[which(consort$name == "randomized_language")] ,
                        label = "Allocation", fill = "#9bc0fc", label.size = NA) +
      ggplot2::annotate("label", x = 10, y = v_arrow_active$yend ,
                        label = "Follow-up", fill = "#9bc0fc", label.size = NA) +
      ggplot2::theme_void()
  }
  
  plot
  
  ggplot2::ggsave(plot, filename = 'enrollment_consort_open.jpg', path = filepath,
                  height = 8.7,
                  width = 14.2,
                  units = "in",
                  device = "jpg")
  
  return(plot)
  
}


