source("global.R")
source("ui.R")
source("modules/data_info_module.R")
source("modules/missing_values_module.R")
source("modules/visualization_module.R")
source("modules/eda_module.R")
source("modules/modeling_module.R")
source("modules/data_table_module.R")

server <- function(input, output, session) {
  data_reactive <- reactive({ data })
  
  dataInfoServer("dataInfo", data = data_reactive, categorical_vars = categorical_vars, continuous_vars = continuous_vars, category_labels = category_labels)
  
  dataTableServer("dataTable", data = data_reactive)
  
  imputed_data <- missingValuesServer("missingValues", data = data_reactive, categorical_vars = categorical_vars, continuous_vars = continuous_vars)
  
  visualizationServer("visualization", data = imputed_data, categorical_vars = categorical_vars, continuous_vars = continuous_vars, category_labels = category_labels, variable_labels = variable_labels)
  
  partitioned_data <- edaServer("eda", data = imputed_data, category_labels = category_labels)
  
  modelingServer("modeling", data = partitioned_data, category_labels = category_labels, continuous_vars = continuous_vars)
}

