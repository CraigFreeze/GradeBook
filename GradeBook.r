#Library Imports
library(tidyverse)
library(dplyr)
#---------------------------------------------------
#------------FUNCTION DECLARATIONS------------------
#---------------------------------------------------
#Increases Amount of Points for a given assignment
curve_test <- function(points, assignment) {
  updated_scores <- list() #Declare Variable
  
  #Method Two of Altering Lists with For Loop
  for (x in 1:length(assignment)) {
    new_score <- assignment[x] + points
    updated_scores <- append(updated_scores, new_score)
  }
  #Method Two of Altering Lists with For Loop
  #NOTE: This for loop is intentionally redundant for new practice
  for (x in 1:length(assignment)) {
    assignment[x] <- updated_scores[x]
  }
  gradebook1['Test1'] = assignment
  print(gradebook1)
  return(gradebook1)
}

# -------------------------------------------------------------------------


#Get Assignment Column
#NOTE: An Alternative to this is data$columnName
get_column_vector <- function(variable) {
  column_assignment <- gradebook1 %>%
    select(variable)
  return(column_assignment)
}

print_column <- function(column) {
  print(get_column_vector(column))
}

make_gradebook_from_data_frame <- function() {
  #Create Dataframe Manually: Preliminary Gradebook
  name <- c("Craig", "Tyson", "JaLynn", "Amber")
  assignment1 <- c(100, 95, 90, 85)
  assignment2 <- c(80, 75, 70, 65)
  gradebook <- data.frame(name, assignment1, assignment2)
  return(gradebook)
}

#---------------------------------------------------
#----------END FUNCTION DECLARATIONS----------------
#---------------------------------------------------
gradebook <- make_gradebook_from_data_frame()

str(gradebook)
View(gradebook) #Expect to see a table that resembles the Structure outputed by str()

print(gradebook$assignment1) #Select and display one column at a time using '$'

#Display top student Test Scores
gradebook %>%
  select(name, assignment1) %>%
  filter(assignment1 > 90) %>%
  arrange(assignment1)

#CSV Import
fpath <- "gradebook.csv"
gradebook1 <- read.csv(fpath, header = TRUE)

#Normalize Data from CSV
gradebook1$Year <-
  factor(gradebook1$Year,
         levels = c("Freshman", "Sophmore", "Junior", "Senior")) #define organizational levels
gradebook1$ID <- as.integer(gradebook1$ID)
gradebook1$Imaginary_ID <- as.complex(gradebook1$Imaginary_ID)
gradebook1$Test1 <- as.numeric(gradebook1$Test1)
gradebook1$Test2 <- as.numeric(gradebook1$Test2)
gradebook1$Quiz1 <- as.numeric(gradebook1$Quiz1)
gradebook1$Writing1 <- as.numeric(gradebook1$Writing1)
gradebook1$Lab1 <- as.numeric(gradebook1$Lab1)

str(gradebook1)

#Create new column and report top students
gradebook1 <- gradebook1 %>%
  mutate(
    Analysis = case_when(
      Test1 < 50 ~ 'Unacceptable',
      Test1 < 60 ~ 'Awful',
      Test1 < 70 ~ 'Bad',
      Test1 < 80 ~ 'Okay',
      Test1 < 90 ~ 'Good',
      TRUE ~ 'Excellent'
    )
  )

View(gradebook1)

display_score_change <- function(assignment_changed) {
  #NOTE: data$variable could also be used to retrieve column vector
  assignment <-
    get_column_vector(assignment_changed) #Stores the vector that will be curved
  assignment_old <-
    assignment #Makes a copy for future comparisons
  
  gradebook1 <- curve_test(1000, assignment) #returns dataframe
  
  #SELECT STUDENT, CURVED CATEGORY AND ASSIGNMENT OLD TO COMPARE AND CONTRAST
  curve_comparison <-
    data.frame(gradebook1$Student, assignment_old, gradebook1$Test1)
  
  #Rename - Columns
  colnames (curve_comparison) [1] <-
    "Student"
  colnames (curve_comparison) [2] <-
    "Old"
  colnames (curve_comparison) [3] <-
    "New"
  
  print(colnames(curve_comparison))
  return(curve_comparison)
}

grade_change_table <- display_score_change("Test1")
View(grade_change_table)

#Show the new table that includes Analysis of test1

#CREATE A SUMMARY TABLE
print(names(gradebook1)) #Displays Columns names to choose to summarize
selection <- gradebook1 %>%
  select(Student, Test1, Test2)
summary(selection)

#Display Data Set
boxplot(gradebook1$Test1)
stripchart(gradebook[, 2])
barplot(as.matrix.data.frame(gradebook))
barplot(gradebook$assignment2)

#Math Functions
sum(gradebook1$Test1) #Total points awarded

#Export Assignment View
write.csv(grade_change_table, "gradeChange.csv", row.names = FALSE)
