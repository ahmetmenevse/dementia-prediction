source("modules/data_info_module.R")
source("modules/missing_values_module.R")
source("modules/visualization_module.R")
source("modules/eda_module.R")

ui <- navbarPage(
  theme = shinytheme("cerulean"),
  "Dataset Analysis",
  
  tabPanel("Dataset Information",
           dataInfoUI("dataInfo")
  ),
  
  tabPanel("Missing Values",
           missingValuesUI("missingValues")
  ),
  
  tabPanel('Visualization',
           visualizationUI('visualization')
  ),
  
  tabPanel("EDA",
           edaUI("eda"))
)