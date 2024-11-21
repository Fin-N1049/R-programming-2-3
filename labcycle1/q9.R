run_length_encoding <- function(input_str) {
  encoded_str <- ""
  count <- 1
  
  for (i in 2:nchar(input_str)) {
    if (substr(input_str, i, i) == substr(input_str, i - 1, i - 1)) {
      count <- count + 1
    } else {
      encoded_str <- paste0(encoded_str, substr(input_str, i - 1, i - 1), count)
      count <- 1
    }
  }
  
  encoded_str <- paste0(encoded_str, substr(input_str, nchar(input_str), nchar(input_str)), count)
  
  return(encoded_str)
}

compress_string <- function() {
  input_str <- readline(prompt = "Enter a string to compress using run-length encoding: ")
  compressed_str <- run_length_encoding(input_str)
  cat(sprintf("Compressed string: %s\n", compressed_str))
}

compress_string()
