clean_string <- function(str) {
  str <- tolower(str)
  str <- gsub("[[:punct:]\\s]", "", str)
  return(str)
}

is_palindrome <- function(str) {
  cleaned_str <- clean_string(str)
  reversed_str <- rev(strsplit(cleaned_str, NULL)[[1]])
  reversed_str <- paste(reversed_str, collapse = "")
  return(cleaned_str == reversed_str)
}

check_palindrome <- function() {
  input_str <- readline(prompt = "Enter a string to check if it's a palindrome: ")
  
  if (is_palindrome(input_str)) {
    cat(sprintf("\"%s\" is a palindrome.\n", input_str))
  } else {
    cat(sprintf("\"%s\" is not a palindrome.\n", input_str))
  }
}

check_palindrome()
