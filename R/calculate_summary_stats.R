#' Calculate Summary Statistics with Optional Grouping
#'
#' This function calculates mean, standard deviation, standard error, and the
#' lower and upper bounds of a 95% confidence interval for a specified variable
#' in a data frame. Optionally, it can group the data by a specified variable
#' before performing these calculations.
#'
#' @param data A data frame containing the data to be analyzed.
#' @param variable A string indicating the name of the variable for which to calculate the statistics.
#' @param group An optional string indicating the name of the variable to group the data by.
#'
#' @return A data frame containing the calculated mean, standard deviation, standard error,
#'         and the lower and upper bounds of a 95% confidence interval. If a group is specified,
#'         these statistics are calculated for each group.
#'
#' @details
#' The function first checks if a group is specified and, if so, groups the data accordingly.
#' It then calculates the mean, standard deviation, and count (n) for the specified variable.
#' Next, it computes the standard error and the confidence interval bounds using the t-distribution.
#'
#' @examples
#' # Example without grouping
#' calculate_summary_stats(your_data_frame, "your_variable")
#'
#' # Example with grouping
#' calculate_summary_stats(your_data_frame, "your_variable", "your_group_variable")
#'
#' @importFrom dplyr %>% group_by summarise mutate select
#' @importFrom rlang sym
#' @export
#'
#'
calculate_summary_stats <- function(data, variable, group=NULL) {
  # Check if group is specified and, if so, use it for grouping
  if (!is.null(group)) {
    data <- data %>% group_by(!!rlang::sym(group))
  }

  # Calculate summary statistics
  data %>%
    summarise(
      mean_value = mean(!!rlang::sym(variable), na.rm = TRUE),
      sd_value = sd(!!rlang::sym(variable), na.rm = TRUE),
      n_group = n()
    ) %>%
    mutate(
      se_value = sd_value / sqrt(n_group),
      lower_bound = mean_value - qt(0.975, df = n_group - 1) * se_value,
      upper_bound = mean_value + qt(0.975, df = n_group - 1) * se_value
    ) %>%
    # If group is specified, include it in the selection
    {
      if (!is.null(group)) {
        select(., !!rlang::sym(group), mean_value, sd_value, se_value, lower_bound, upper_bound, n_group)
      } else {
        select(., mean_value, sd_value, se_value, lower_bound, upper_bound, n_group)
      }
    }
}

