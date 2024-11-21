reverse_list <- function(input_list) {
  n <- length(input_list)
  reversed_list <- vector("list", n)
  
  for (i in 1:n) {
    reversed_list[[i]] <- input_list[[n - i + 1]]
  }
  
  return(reversed_list)
}

# Main function
reverse_elements <- function() {
  input_str <- readline(prompt = "Enter elements of the list separated by commas: ")
  input_list <- strsplit(input_str, ",")[[1]]
  
  reversed_list <- reverse_list(input_list)
  cat("Reversed list:", paste(reversed_list, collapse = ", "), "\n")
}

reverse_elements()
