# Function to validate age
validate_age <- function(age) {
  if (!is.na(as.integer(age)) && as.integer(age) > 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Function to validate grade
validate_grade <- function(grade) {
  valid_grades <- c("A", "B", "C", "D", "F")
  if (grade %in% valid_grades) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Function to read student records
read_student_records <- function() {
  students <- data.frame(Name = character(), Age = integer(), Grade = character(), stringsAsFactors = FALSE)
  
  repeat {
    cat("Enter student name (or 'done' to finish): ")
    name <- readline()
    if (tolower(name) == 'done') {
      break
    }
    
    cat("Enter student age: ")
    age <- readline()
    if (!validate_age(age)) {
      cat("Invalid age. Please enter a positive integer.\n")
      next
    }
    
    cat("Enter student grade (A, B, C, D, F): ")
    grade <- readline()
    if (!validate_grade(grade)) {
      cat("Invalid grade. Please enter a valid letter grade (A, B, C, D, F).\n")
      next
    }
    
    students <- rbind(students, data.frame(Name = name, Age = as.integer(age), Grade = grade, stringsAsFactors = FALSE))
  }
  
  return(students)
}

# Function to calculate average age of students with valid records
calculate_average_age <- function(students) {
  if (nrow(students) == 0) {
    return(NA)
  }
  
  valid_ages <- students$Age
  average_age <- mean(valid_ages)
  return(average_age)
}

# Main program
students <- read_student_records()
if (nrow(students) > 0) {
  average_age <- calculate_average_age(students)
  cat("Average age of students with valid records:", average_age, "\n")
} else {
  cat("No valid student records to process.\n")
}
