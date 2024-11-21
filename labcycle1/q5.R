sum_series <- function() {
  n <- as.integer(readline(prompt = "Enter the number of terms: "))
  
  if (is.na(n) || n <= 0) {
    cat("Please enter a valid positive integer for the number of terms.\n")
    return()
  }
  
  sum <- 0
  sign <- 1
  
  for (i in 1:n) {
    term <- sign * (i / (2 * i - 1))
    sum <- sum + term
    sign <- -sign
  }
  
  cat(sprintf("The sum of the series up to %d terms is: %f\n", n, sum))
}

sum_series()
