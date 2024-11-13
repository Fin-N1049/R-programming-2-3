# Function to count the total number of words
count_words <- function(text) {
  words <- unlist(strsplit(text, "\\s+"))
  return(length(words))
}

# Function to calculate the average word length
average_word_length <- function(text) {
  words <- unlist(strsplit(text, "\\s+"))
  word_lengths <- nchar(words)
  average_length <- mean(word_lengths)
  return(average_length)
}

# Function to identify and print the longest word
longest_word <- function(text) {
  words <- unlist(strsplit(text, "\\s+"))
  longest <- words[which.max(nchar(words))]
  return(longest)
}

# Function to replace all occurrences of a specific word with another word
replace_word <- function(text, old_word, new_word) {
  modified_text <- gsub(old_word, new_word, text, ignore.case = TRUE)
  return(modified_text)
}

# Main function to perform all tasks
main_function <- function(paragraph, old_word, new_word) {
  total_words <- count_words(paragraph)
  avg_word_length <- average_word_length(paragraph)
  longest <- longest_word(paragraph)
  modified_text <- replace_word(paragraph, old_word, new_word)
  
  cat("Total number of words:", total_words, "\n")
  cat("Average word length:", avg_word_length, "\n")
  cat("Longest word:", longest, "\n")
  cat("Modified text:\n", modified_text, "\n")
}

# Input paragraph of text
paragraph <- "My name is Shiyas and I am coming from Malappuram."

# Words to replace
old_word <- "Malappuram"
new_word <- "Perinthalmanna"

# Call the main function
main_function(paragraph, old_word, new_word)
