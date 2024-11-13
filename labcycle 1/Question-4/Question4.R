generate_password <- function(length) {
  upper_case <- unlist(strsplit("ABCDEFGHIJKLMNOPQRSTUVWXYZ", NULL))
  lower_case <- unlist(strsplit("abcdefghijklmnopqrstuvwxyz", NULL))
  digits <- unlist(strsplit("0123456789", NULL))
  special_chars <- unlist(strsplit("!@#$%^&*()-_=+[]{}|;:,.<>?/", NULL))
  
  all_chars <- c(upper_case, lower_case, digits, special_chars)
  
  password <- c(
    sample(upper_case, 1),
    sample(lower_case, 1),
    sample(digits, 1),
    sample(special_chars, 1)
  )
  
  remaining_length <- length - 4
  if (remaining_length > 0) {
    password <- c(password, sample(all_chars, remaining_length, replace = TRUE))
  }
  
  password <- sample(password)
  
  password <- paste(password, collapse = "")
  
  return(password)
}

cat("Enter the desired length of the password: ")
password_length <- as.integer(readline())

if (is.na(password_length) || password_length < 4) {
  cat("Invalid input. Password length must be an integer greater than or equal to 4.\n")
} else {
  password <- generate_password(password_length)
  
  cat("Generated password:", password, "\n")
}
