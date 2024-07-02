library(shiny)
library(shinythemes)
library(dplyr)
library(VIM)

data <- read.csv('dataset ICT583 2024S1.csv')

variables <- names(data)[-1]

categorical_vars <- c('Gender', 'Education_ID', 'Financial_status', 'Independent_or_depend_on_family', 'Marital_status_ID', 'MMSE_class_binary')
continuous_vars <- setdiff(variables, categorical_vars)

category_labels <- list(
  Gender = c("0" = "Male", "1" = "Female"),
  Education_ID = c("1" = "Primary School", "2" = "Secondary School", "3" = "High School", "4" = "University"),
  Financial_status = c("1" = "Low", "2" = "Medium-Low", "3" = "Medium", "4" = "Medium-High", "5" = "High"),
  Independent_or_depend_on_family = c("0" = "Independent", "1" = "Dependent"),
  Marital_status_ID = c("1" = "Single", "2" = "Married", "3" = "Widowed", "4" = "Divorced", "5" = "Separated", "6" = "Other"),
  MMSE_class_binary = c("0" = "No Cognitive Impairment", "1" = "Cognitive Impairment")
)
