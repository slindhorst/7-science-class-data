# install packages
install.packages(c("tidyverse", "apaTables"))

# add libraries
library(tidyverse)
library(apaTables)
library(sjPlot)
library(readxl)
library(dataedu)

# Pre-survey for the F15 and S16 semesters
pre_survey_ <- dataedu::pre_survey
print(pre_survey)


# Gradebook and log-trace data for F15 and S16 semesters
course_datra <- dataedu::course_data
print(course_data)

#Log-trace data for F15 and S16 semesters - this is for time spent
course_minutes <- dataedu::course_minutes
print(course_minutes)

pre_survey <-
  pre_survey %>%
  #' Rename the questions something easier to work with because R is case sensitive
  #' and working with variable names in mix case is prone to error
  rename(    q1 = Q1MaincellgroupRow1,
             q2 = Q1MaincellgroupRow2,
             q3 = Q1MaincellgroupRow3,
             q4 = Q1MaincellgroupRow4,
             q5 = Q1MaincellgroupRow5,
             q6 = Q1MaincellgroupRow6,
             q7 = Q1MaincellgroupRow7,
             q8 = Q1MaincellgroupRow8,
             q9 = Q1MaincellgroupRow9,
             q10 = Q1MaincellgroupRow10
  ) %>%
  
  #Convert all question responses to numeric
  mutate_at(vars(q1:q10), list( ~ as.numeric(.)))

pre_survey
  
# This part of the code is where we write the function:
# Function for reversing scales 
reverse_scale <- function(question) {
  #' Reverses the response scales for consistency
  #' Arguments:
  #'    question - survey question
  #' Returns:
  #'    a numeric converted response
  #' Note: even though 3 is not transformed, case_when expects a match for all
  #' possible conditions, so it's best practice to label each possible input
  #' and use TRUE ~ as the final statement returning NA for unexpected inputs
x <- case_when(
  question == 1 ~ 5,
  question == 2 ~ 4,
  question == 3 ~ 3,
  question == 4 ~ 2,
  question == 5 ~ 1,
  TRUE ~ NA_real_
)

x

}

# And here's where we use that function to reverse the scales
# We use the pipe operator %>% here
# Reverse scale for questions 4 and 7
pre_survey <-
  pre_survey %>% 
  mutate(q4 = reverse_scale(q4),
         q7 = reverse_scale(q7))
print(pre_survey)
