#' Preprocess text for analysis
#'
#' @param text A character string containing the text to preprocess
#'
#' @return A character vector containing the preprocessed text
#' @export
#'
#' @examples
#' text_preprocessing("I love building plastic model kits")
text_preprocessing <- function(text) {
  library(tidytext)

  # Create a tibble with the input text
  text_tbl <- tibble(line = 1, text = text)

  # Unnest the text into individual words
  text_tbl <- text_tbl %>%
    unnest_tokens(word, text)

  # Remove stopwords
  text_tbl <- text_tbl %>%
    anti_join(stop_words)

  # Stem the words
  text_tbl$word <- SnowballC::wordStem(text_tbl$word)

  # Return the preprocessed text
  text_tbl$word
}
