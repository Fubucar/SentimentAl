#' Perform sentiment analysis per sentence on text data
#'
#' @param text A character vector containing the text to analyze
#'
#' @return A tibble with the sentiment scores for each sentence
#' @export
#'
#' @examples
#' text_classification("I love playing video games! The alternative I dislike are textbooks, they are boring.")
text_classification <- function(text) {
  library(tidytext)
  library(sentimentr)

  # Convert input text to a tibble with one row per sentence
  sentences <- tibble(text = text) %>%
    unnest_tokens(sentence, text, "sentences")

  # Compute sentiment scores for each sentence
  scores <- sentences %>%
    mutate(sentiment = sentiment_by(sentence, by = NULL)) %>%
    select(sentence, sentiment)

  return(scores)
}
