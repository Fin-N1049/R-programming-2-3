generate_fibonacci_twist <- function() {
  n <- as.integer(readline(prompt = "Enter the number of terms: "))
  
  if (is.na(n) || n <= 0) {
    cat("Please enter a valid positive integer for the number of terms.\n")
    return()
  }
  
  if (n == 1) {
    cat("The Fibonacci series with a twist is: 0\n")
    return()
  } else if (n == 2) {
    cat("The Fibonacci series with a twist is: 0, 1\n")
    return()
  } else if (n == 3) {
    cat("The Fibonacci series with a twist is: 0, 1, 1\n")
    return()
  }
  
  fib <- numeric(n)
  fib[1] <- 0
  fib[2] <- 1
  fib[3] <- 1
  
  for (i in 4:n) {
    fib[i] <- fib[i - 1] + fib[i - 2] + fib[i - 3]
  }
  
  cat("The Fibonacci series with a twist is:", paste(fib, collapse = ", "), "\n")
}

generate_fibonacci_twist()
