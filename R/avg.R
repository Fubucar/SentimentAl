#' Calculate the average word length in a character vector
#'
#' @param text A character vector containing the text to calculate the average word length from
#'
#' @return The average word length as a numeric value
#' @export
#'
#' @examples
#' calculate_avg_word_length("The quick brown fox jumps over the lazy dog.")
calculate_avg_word_length <- function(text) {
  words <- strsplit(text, " ")[[1]]
  avg_length <- mean(nchar(words))
  return(avg_length)
}
