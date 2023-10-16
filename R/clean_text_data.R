#' Clean and Standardize Text Data
#'
#' This function standardizes text by converting it to lowercase, removing extra spaces,
#' stripping out email addresses and URLs, and handling non-ASCII characters.
#'
#' @param data A data frame containing the text data.
#' @param text_column The name of the column (as a string) containing the text data.
#'
#' @return A data frame with the cleaned text in a new column named `cleaned_text`.
#' @export
#'
#' @examples
#' # Create a sample data frame
#' df <- data.frame(text = c("My email is example@email.com", "Visit http://example.com for more info"))
#' clean_text_data(df, "text")
#'
clean_text_data <- function(data, text_column) {
  library(dplyr)
  library(stringr)

  data %>%
    mutate(cleaned_text = str_to_lower(!!sym(text_column))) %>%
    mutate(cleaned_text = str_squish(cleaned_text)) %>%
    mutate(cleaned_text = gsub("\\S+@\\S+\\.[a-z]+", "", cleaned_text)) %>%
    mutate(cleaned_text = gsub("http[^[:space:]]*", "", cleaned_text)) %>%
    mutate(cleaned_text = iconv(cleaned_text, to = "ASCII//TRANSLIT"))
}
