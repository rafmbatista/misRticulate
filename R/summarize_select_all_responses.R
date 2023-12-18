#' Summarize 'Select All' Survey Responses
#'
#' This function compiles and summarizes indicator variables from survey data where participants could "Select All" responses that applied. It then computes the proportion of selections and their standard errors for each response option.
#'
#' @param .data A dataframe containing the survey data.
#' @param prefix The common prefix of the 'Select All' indicator variables in the dataset.
#' @param renaming_scheme (Optional) A named list or vector for renaming the extracted categories. If not provided, categories won't be renamed.
#' @param conf.lvl (Optional) The confidence level for the confidence intervals. Default is 0.95.
#' @param decimal.digits (Optional) The number of decimal digits to round the results to. Default is 3.
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
summarize_select_all_responses <- function(.data,
                                           prefix,
                                           conf.lvl = 0.95,
                                           decimal.digits = 3,
                                           renaming_scheme = NULL) {

  # Confidence level tails
  tails <- 1 - ((1 - conf.lvl) / 2)

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
      n = sum(selection, na.rm = TRUE), # Number of selections
      prop.total = mean(selection, na.rm = TRUE),
      SE = plotrix::std.error(selection, na.rm = TRUE), # Standard error
      lower = prop.total - (qnorm(tails) * SE), # Lower bound of the confidence interval
      upper = prop.total + (qnorm(tails) * SE)  # Upper bound of the confidence interval
    ) %>%
    mutate(across(c("prop.total", "SE", "lower", "upper"),
                  ~round(.x, digits = decimal.digits)) # Round results
           ) %>%
    arrange(desc(prop.total))

  # Rename response options if a renaming scheme is provided
  if (!is.null(renaming_scheme)) {
    summarized_data$response_option <- factor(summarized_data$response_option, levels = names(renaming_scheme))
    levels(summarized_data$response_option) <- renaming_scheme
  }

  return(summarized_data)
}

