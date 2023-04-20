#' Perform sentiment analysis on a text input
#'
#' @param text A character string containing the text to analyze
#'
#' @return A numeric value indicating the overall sentiment of the text
#' @export
#'
#' @examples
#' sentiment_analysis("Hello how are you I'm doing great thanks love")
sentiment_analysis <- function(text) {
  library(tidytext)
  library(textdata)

  # Create a tibble with the input text
  text_tbl <- tibble(line = 1, text = text)

  # Unnest the text into individual words
  text_tbl <- text_tbl %>%
    unnest_tokens(word, text)

  # Calculate the sentiment score for each word
  sentiment_scores <- text_tbl %>%
    inner_join(get_sentiments("afinn")) %>%
    group_by(line) %>%
    summarize(sentiment = sum(value))

  # Return the overall sentiment score
  sentiment_scores$sentiment
}
