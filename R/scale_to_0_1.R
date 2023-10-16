#' Scale a variable to the range [0, 1]
#'
#' This function scales (or normalizes) a variable such that its minimum value becomes 0 
#' and its maximum value becomes 1. Any other values within the variable are scaled 
#' proportionally between 0 and 1.
#'
#' The formula for scaling is:
#' \deqn{\frac{{variable - min(variable)}}{{max(variable) - min(variable)}}}
#' where:
#' \itemize{
#'   \item \code{variable} is the original numeric vector.
#'   \item \code{min(variable)} is the minimum value of the original numeric vector.
#'   \item \code{max(variable)} is the maximum value of the original numeric vector.
#' }
#'
#' @param variable A numeric vector to be scaled to [0, 1].
#'
#' @return A numeric vector of the same length as `variable`, with values scaled to [0, 1].
#' @examples
#' scale_to_0_1(c(10, 20, 30, 40, 50))
#' scale_to_0_1(c(-2, -1, 0, 1, 2))
#'
scale_to_0_1 <- function(variable) {
  (variable - min(variable, na.rm = TRUE)) / (max(variable, na.rm = TRUE) - min(variable, na.rm = TRUE))
}
