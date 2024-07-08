library(shiny)
library(shinythemes)
library(dplyr)
library(VIM)

data <- read.csv('dataset ICT583 2024S1.csv', stringsAsFactors = FALSE)

if (is.na(names(data)[1]) || names(data)[1] == "") {
  names(data)[1] <- "ID"
}

variables <- names(data)[-1]

categorical_vars <- c('Gender', 'Education_ID', 'Financial_status', 'Independent_or_depend_on_family', 'Marital_status_ID', 'MMSE_class_binary')
continuous_vars <- setdiff(variables, categorical_vars)

category_labels <- list(
  Gender = c("0" = "Female", "1" = "Male"),
  Education_ID = c("1" = "No Education", "2" = "Primary School", "3" = "Secondary School", "4" = "University"),
  Financial_status = c("1" = "Independent", "2" = "CSSA", "3" = "Disability Allowance", "4" = "Old Age Allowance", "5" = "Independent with Old Age Allowance", '6' = 'Independent with Disability Allowance', '7' = 'Independent with CSSA', '8' = 'Other'),
  Independent_or_depend_on_family = c("0" = "Independent", "1" = "Dependent"),
  Marital_status_ID = c("1" = "Single", "2" = "Married", "3" = "Widowed", "4" = "Separated", "5" = "Divorced", "6" = "Married but not live with spouse"),
  MMSE_class_binary = c("0" = "No Dementia Risk", "1" = "Possible Dementia Risk")
)

variable_labels <- list(
  Gender = "Gender",
  Education_ID = "Education Level",
  Financial_status = "Financial Status",
  Independent_or_depend_on_family = "Independent or Dependent",
  Marital_status_ID = "Marital Status",
  MMSE_class_binary = "MMSE Class (Binary)",
  Age = "Age",
  Body_Height = "Height",
  Body_Weight = "Weight",
  MNAa_total = "MNAa total",
  MNAb_total = "MNAb total",
  GDS = 'GDS'
)
