# install packages
install.packages(c("tidyverse", "apaTables"))

# add libraries
library(tidyverse)
library(apaTables)
library(sjPlot)
library(readxl)
library(dataedu)

# Pre-survey for the F15 and S16 semesters
pre_survey <- dataedu::pre_survey
print(pre_survey)


# Gradebook and log-trace data for F15 and S16 semesters
course_datra <- dataedu::course_data
print(course_data)

#Log-trace data for F15 and S16 semesters - this is for time spent
course_minutes <- dataedu::course_minutes
print(course_minutes)

pre_survey <-
  pre_survey %>%
  # Rename the questions something easier to work with because R is # and working with variable names in mix case is prone to error