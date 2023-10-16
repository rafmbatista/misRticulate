#' Summarize Categorical Variables with Confidence Intervals
#'
#' This function creates a summary table for categorical variables, which includes
#' proportions, standard errors, and confidence intervals.
#'
#' @param .data A data frame containing the variable to be summarized.
#' @param variable The variable to be summarized (as a symbol).
#' @param conf.lvl Confidence level for the interval calculation (default is 0.95).
#' @param arrange Logical. Should the result be arranged in descending order of proportions? (default is FALSE).
#' @param decimal.digits Number of decimal digits to round the results (default is 3).
#'
#' @return A data frame with the summarized values.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(category = c('A', 'A', 'B', 'B', 'B', 'C'))
#' summarize_categories(df, category)
#' }
summarize_categories <- function(.data, variable,
                                 conf.lvl = 0.95,
                                 arrange = FALSE,
                                 decimal.digits = 3) {

  # Confidence level tails
  tails <- 1 - ((1 - conf.lvl) / 2)

  # Summarize the data
  table <- .data %>%
    group_by({{variable}}) %>%
    summarize(n = n(), .groups = "drop") %>%  # Count occurrences
    mutate(
      prop.total = n / sum(n),
      SE = sqrt((prop.total * (1 - prop.total)) / sum(n)), # Standard Error
      lower = prop.total - (qnorm(tails) * SE), # Lower bound of the confidence interval
      upper = prop.total + (qnorm(tails) * SE)  # Upper bound of the confidence interval
    ) %>%
    mutate(across(c("prop.total", "SE", "lower", "upper"),
                  ~round(.x, digits = decimal.digits)))  # Round results

  # Optionally, arrange the table
  if (arrange) {
    table <- table %>% arrange(desc(prop.total))
  }

  return(table)
}
