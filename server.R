source("global.R")
source("modules/data_info_module.R")
source("modules/missing_values_module.R")
source("modules/visualization_module.R")

server <- function(input, output, session) {
  data_reactive <- reactive({ data })
  
  dataInfoServer("dataInfo", data = data_reactive, categorical_vars = categorical_vars, continuous_vars = continuous_vars, category_labels = category_labels)
  
  # Imputed veriyi missingValuesServer'dan al
  imputed_data <- missingValuesServer("missingValues", data = data_reactive, categorical_vars = categorical_vars, continuous_vars = continuous_vars)
  
  # Imputed veriyi visualizationServer'a aktar
  visualizationServer("visualization", data = imputed_data, categorical_vars = categorical_vars, continuous_vars = continuous_vars, category_labels = category_labels, variable_labels = variable_labels)
}

