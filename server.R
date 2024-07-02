source("global.R")
source("modules/data_info_module.R")
source("modules/missing_values_module.R")

server <- function(input, output, session) {
  dataInfoServer("dataInfo", data = data, categorical_vars = categorical_vars, continuous_vars = continuous_vars, category_labels = category_labels)
  missingValuesServer("missingValues", data = data)
}
