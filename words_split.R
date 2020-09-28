words_split <- function(vector_of_words, sep = ", "){
  # Function that takes a character vector in which some elements have more
  # than one word, and a character variable of the separator and outputs a
  # character vector of words in which each element corresponds to a single
  # word elements. Multiple words in an element are assumed to be separated
  # by sep, the specified separator.
  library(stringr)
  
  # Concatenate all elements into one large string with two spaces between
  # different elements
  vector_of_words <- str_c(as.character(vector_of_words),
                           sep = "  ", collapse = "  ")
  
  # Replace all of the multiple spaces with "; ", don't replace the single
  # spaces
  vector_of_words <- str_replace_all(vector_of_words, "\\s\\s+", sep)
  
  # Return the vectorized form of the long character variable
  return( unlist(str_split(vector_of_words, pattern = sep)) )
}