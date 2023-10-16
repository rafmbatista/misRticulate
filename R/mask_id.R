#' Mask an Input String
#'
#' This function masks a given input string to protect sensitive data such as IDs.
#' It works by shuffling the input string characters and then computing its MD5 hash.
#' This method ensures a consistent hash for a given string when the same seed is used,
#' allowing for reproducibility while ensuring privacy.
#'
#' @param string A character string to be masked.
#' @param seed A single value, interpreted as an integer, to set the seed of
#'             R's random number generator for reproducibility. Default is 395.
#'
#' @return An MD5 hashed representation of the masked string.
#'
#' @seealso
#' \href{https://researchbox.org/messages.php?id=replace_private_id}{Masking IDs in ResearchBox}
#'
#' @examples
#' mask("sampleID")
#' mask("sampleID", seed = 123)
#'
mask_id <- function(string, seed = 395) {
  set.seed(seed)
  stuff = c(letters, LETTERS, 0:9)
  new = chartr(paste0(stuff, collapse = ''), paste0(sample(stuff), collapse = ''), string)
  md5 = openssl::md5(new)
  return(md5)
}
