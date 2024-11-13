encrypt_char <- function(char, shift) {
  if (grepl("[A-Z]", char)) {
    # Uppercase letter
    base <- as.numeric(charToRaw('A'))
    char_shifted <- (as.numeric(charToRaw(char)) - base + shift) %% 26 + base
    return(rawToChar(as.raw(char_shifted)))
  } else if (grepl("[a-z]", char)) {
    # Lowercase letter
    base <- as.numeric(charToRaw('a'))
    char_shifted <- (as.numeric(charToRaw(char)) - base + shift) %% 26 + base
    return(rawToChar(as.raw(char_shifted)))
  } else {
    # Non-alphabetic character, return as is
    return(char)
  }
}

# Function to encrypt a sentence using Caesar cipher
encrypt_sentence <- function(sentence, shift) {
  encrypted_sentence <- sapply(strsplit(sentence, NULL)[[1]], encrypt_char, shift)
  return(paste0(encrypted_sentence, collapse = ""))
}

# Read input from the user
cat("Enter a sentence to encrypt: ")
sentence <- readline()

cat("Enter the shift value: ")
shift_value <- as.integer(readline())

# Encrypt the sentence
encrypted_sentence <- encrypt_sentence(sentence, shift_value)

# Print the encrypted sentence
cat("Encrypted sentence:", encrypted_sentence, "\n")
