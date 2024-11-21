is_prime <- function(num) {
  if (num <= 1) {
    return(FALSE)
  }
  for (i in 2:sqrt(num)) {
    if (num %% i == 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}

check_single_number <- function() {
  num <- as.integer(readline(prompt = "Enter a number to check if it is prime: "))
  if (is.na(num)) {
    cat("Please enter a valid integer.\n")
  } else if (is_prime(num)) {
    cat(sprintf("%d is a prime number.\n", num))
  } else {
    cat(sprintf("%d is not a prime number.\n", num))
  }
}

find_primes_in_range <- function() {
  start <- as.integer(readline(prompt = "Enter the start of the range: "))
  end <- as.integer(readline(prompt = "Enter the end of the range: "))
  
  if (is.na(start) || is.na(end)) {
    cat("Please enter valid integers for the range.\n")
    return()
  }
  
  if (start > end) {
    cat("The start of the range must be less than or equal to the end of the range.\n")
    return()
  }
  
  primes <- c()
  for (num in start:end) {
    if (is_prime(num)) {
      primes <- c(primes, num)
    }
  }
  
  if (length(primes) == 0) {
    cat(sprintf("There are no prime numbers in the range %d to %d.\n", start, end))
  } else {
    cat(sprintf("The prime numbers in the range %d to %d are: %s\n", start, end, paste(primes, collapse = ", ")))
  }
}

main <- function() {
  repeat {
    cat("\nPrime Number Checker Menu:\n")
    cat("1. Check if a single number is prime\n")
    cat("2. Find all primes in a range\n")
    cat("3. Exit\n")
    choice <- as.integer(readline(prompt = "Enter your choice: "))
    
    if (is.na(choice)) {
      cat("Please enter a valid choice (1, 2, or 3).\n")
      next
    }
    
    if (choice == 1) {
      check_single_number()
    } else if (choice == 2) {
      find_primes_in_range()
    } else if (choice == 3) {
      cat("Exiting the program.\n")
      break
    } else {
      cat("Invalid choice. Please enter 1, 2, or 3.\n")
    }
  }
}

main()
