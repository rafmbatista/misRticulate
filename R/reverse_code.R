#' Reverse code a variable based on specified original range
#'
#' This function reverse codes a variable so that the original minimum becomes the maximum and vice versa.
#' It checks to ensure that all values in the variable are within the specified original range and handles potential NA values.
#'
#' @param variable A numeric vector to be reverse coded.
#' @param min_orig The original minimum value of the range.
#' @param max_orig The original maximum value of the range.
#'
#' @return A numeric vector of the same length as `variable`, with values reverse coded.
#' @examples
#' reverse_code(c(1, 2, 3, 4, 5), 1, 5)
#' reverse_code(c(-2, -1, 0, 1, 2), -2, 2)
#'
reverse_code <- function(variable, min_orig, max_orig) {
  # Check if any value in the variable is outside the specified range
  if (any(variable < min_orig, na.rm = TRUE) | any(variable > max_orig, na.rm = TRUE)) {
    stop("Some values in the variable are outside the specified original range.")
  }
  
  # Compute the reversed variable
  max_orig + min_orig - variable
}


