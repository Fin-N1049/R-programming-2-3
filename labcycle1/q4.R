password <- function(){
  
  len <- as.integer(readline(prompt = "Enter the password length: "))
  
  upper <- LETTERS
  lower <- letters
  digits <- 0:9
  special <- c('!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '-', '_', '=', '+', '~')
  
  full <- c(upper, lower, digits, special)
  
  pass <- sample(full, len, replace = TRUE)
  pass <- paste0(pass, collapse="")
  cat(sprintf("The generated password is: %s", pass))
}


password()
