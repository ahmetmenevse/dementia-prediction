source("modules/data_info_module.R")
source("modules/missing_values_module.R")
source("modules/visualization_module.R")

ui <- navbarPage(
  theme = shinytheme("cerulean"),
  "Dataset Analysis",
  
  tabPanel("Data set Information",
           dataInfoUI("dataInfo")
  ),
  
  tabPanel("Missing Values",
           missingValuesUI("missingValues")
  ),
  
  tabPanel('Dementia Risk Prediction Visualization',
           visualizationUI('visualization')
  )
)