#' Summarize 'Select All' Survey Responses
#'
#' This function compiles and summarizes indicator variables from survey data where participants could "Select All" responses that applied. It then computes the proportion of selections and their standard errors for each response option.
#'
#' @param .data A dataframe containing the survey data.
#' @param prefix The common prefix of the 'Select All' indicator variables in the dataset.
#' @param renaming_scheme (Optional) A named list or vector for renaming the extracted categories. If not provided, categories won't be renamed.
#'
#' @return A dataframe with columns for the response option, proportion of selections, and standard error of the proportions.
#' @examples
#' \dontrun{
#' data_clean <- data.frame(
#'   select_all_type_of_conv_describeIdea = sample(c(0,1), 100, replace = TRUE),
#'   select_all_type_of_conv_describeEmotion = sample(c(0,1), 100, replace = TRUE)
#' )
#' summarized_data <- summarize_select_all_responses(data_clean, "select_all_type_of_conv")
#' print(summarized_data)
#' }
#'
summarize_select_all_responses <- function(.data, prefix, renaming_scheme = NULL) {
  # Filter columns that match the prefix and don't contain "TEXT"
  summarized_data <- .data %>%
    select(starts_with(prefix), -contains("TEXT")) %>%
    pivot_longer(
      cols = everything(),
      names_to = "response_option",
      names_prefix = paste0(prefix, "_"),
      values_to = "selection"
    ) %>%
    group_by(response_option) %>%
    summarize(
      prop_select = mean(selection, na.rm = TRUE),
      SE = plotrix::std.error(selection, na.rm = TRUE)
    ) %>%
    arrange(desc(prop_select))

  # Rename response options if a renaming scheme is provided
  if (!is.null(renaming_scheme)) {
    summarized_data$response_option <- factor(summarized_data$response_option, levels = names(renaming_scheme))
    levels(summarized_data$response_option) <- renaming_scheme
  }

  return(summarized_data)
}

