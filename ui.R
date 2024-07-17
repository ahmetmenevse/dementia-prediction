source("modules/data_info_module.R")
source("modules/missing_values_module.R")
source("modules/visualization_module.R")
source("modules/eda_module.R")
source("modules/modeling_module.R")
source("modules/data_table_module.R")

ui <- navbarPage(
  theme = shinytheme("cerulean"),
  "Dementia Risk Prediction",
  
  tabPanel("1-Dataset Information",
           dataInfoUI("dataInfo")
  ),
  
  tabPanel("2-Data Table",
           dataTableUI("dataTable")
  ),
  
  tabPanel("3-Missing Values",
           missingValuesUI("missingValues")
  ),
  
  tabPanel('4-Visualization',
           visualizationUI('visualization')
  ),
  
  tabPanel("5-Exploratory Data Analysis",
           edaUI("eda")
  ),
  tabPanel("6-Modeling",
           modelingUI("modeling")
  )
)