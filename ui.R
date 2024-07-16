source("modules/data_info_module.R")
source("modules/missing_values_module.R")
source("modules/visualization_module.R")
source("modules/eda_module.R")
source("modules/modeling_module.R")
source("modules/data_table_module.R")

ui <- navbarPage(
  theme = shinytheme("cerulean"),
  "Dataset Analysis",
  
  tabPanel("Dataset Information",
           dataInfoUI("dataInfo")
  ),
  
  tabPanel("Data Table",
           dataTableUI("dataTable")
  ),
  
  tabPanel("Missing Values",
           missingValuesUI("missingValues")
  ),
  
  tabPanel('Visualization',
           visualizationUI('visualization')
  ),
  
  tabPanel("Exploratory Data Analysis",
           edaUI("eda")
  ),
  tabPanel("Modeling",
           modelingUI("modeling")
  )
)