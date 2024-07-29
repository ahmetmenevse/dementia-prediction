# Dementia Risk Prediction

## Overview

The Dementia Risk Prediction app is designed to analyze and predict dementia risk using various machine learning models. This project includes steps for data preprocessing, visualization, exploratory data analysis, and model training.

## Application Link

You can access the deployed application [here](https://nickless.shinyapps.io/Dementia_prediction/).

## Dataset

The dataset is sourced from a mobile healthcare service operated by non-governmental organizations managing elderly care centers in Hong Kong between 2008-2018. It includes information on various demographics, health, and quality of life for elderly individuals.

- **Observations**: 2,299
- **Variables**: 12, covering:
  - Age
  - Gender
  - Body_Height
  - Body_Weight
  - Education_ID
  - Financial_status
  - GDS: (Geriatric Depression Scale)
  - Independent_or_depend_on_family
  - Marital_status_ID
  - MNAa_total (Mini Nutritional Assessment part A)
  - MNAb_total (Mini Nutritional Assessment part B)
  - MMSE_class_binary (Mini Mental State Exam class, indicating potential dementia risk)

  
Note: This dataset is a subset of the original data used in a comprehensive study. For detailed information and methodologies, refer to the published research [article](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7490674/)

## Features

1. **Dataset Information**: Provides a detailed description of the dataset and summaries of each variable.
2. **Data Table**: Displays the entire dataset with options to search and filter the data.
3. **Missing Values**: Allows users to impute missing values using various methods (Mean, Median, KNN, Mode, Hot Deck, Delete).
4. **Visualization**: Offers visualizations for both categorical and continuous variables.
5. **Exploratory Data Analysis (EDA)**: Includes correlation analysis, scaling, and data partitioning.
6. **Modeling**: Enables users to select variables and models (Logistic Regression, Random Forest, SVM), train the models, and view model training results, confusion matrices, and model comparison metrics.

## Usage

### Dataset Information

- View dataset description and variable summaries.

### Data Table

- Explore the dataset with options to search and filter entries.

### Missing Values

- Impute missing values using selected methods and view summaries and imputation details.

### Visualization

- Visualize categorical and continuous variables.

### Exploratory Data Analysis (EDA)

- Perform correlation analysis.
- Scale data using standardization or normalization.
- Partition data into training and testing sets.

### Modeling

- Select variables and models for training.
- View model training results, confusion matrices, and comparison metrics.

## Acknowledgements

The dataset used in this project is provided by [Murdoch University](https://www.murdoch.edu.au) for the ICT583 Data Science Applications. The original research included more variables and employed different methods to analyze the data. For detailed information and methodologies, please refer to the published research [article](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7490674/)

## References

- [R](https://cran.r-project.org/)
- [RStudio](https://posit.co/download/rstudio-desktop/) 
- [Shiny](https://shiny.posit.co/)
