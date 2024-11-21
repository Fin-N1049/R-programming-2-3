# Function to check if age is a positive integer
is_valid_age <- function(age) {
  return(!is.na(as.integer(age)) && as.integer(age) > 0)
}

# Function to check if grade is valid
is_valid_grade <- function(grade) {
  valid_grades <- c("A", "B", "C", "D", "F")
  return(grade %in% valid_grades)
}

# Function to read student records
read_student_records <- function() {
  students <- data.frame(Name = character(), Age = integer(), Grade = character(), stringsAsFactors = FALSE)
  
  repeat {
    name <- readline(prompt = "Enter student's name (or 'done' to finish): ")
    if (tolower(name) == "done") {
      break
    }
    
    age <- readline(prompt = "Enter student's age: ")
    while (!is_valid_age(age)) {
      cat("Invalid age. Please enter a positive integer.\n")
      age <- readline(prompt = "Enter student's age: ")
    }
    age <- as.integer(age)
    
    grade <- readline(prompt = "Enter student's grade (A, B, C, D, F): ")
    while (!is_valid_grade(grade)) {
      cat("Invalid grade. Please enter a valid grade (A, B, C, D, F).\n")
      grade <- readline(prompt = "Enter student's grade (A, B, C, D, F): ")
    }
    
    students <- rbind(students, data.frame(Name = name, Age = age, Grade = grade, stringsAsFactors = FALSE))
  }
  
  return(students)
}

# Function to calculate average age of students with valid records
calculate_average_age <- function(students) {
  if (nrow(students) == 0) {
    return(NA)
  }
  
  return(mean(students$Age))
}

# Main script
students <- read_student_records()
if (nrow(students) == 0) {
  cat("No valid student records entered.\n")
} else {
  cat("Student records:\n")
  print(students)
  
  avg_age <- calculate_average_age(students)
  cat("Average age of students with valid records:", avg_age, "\n")
}
