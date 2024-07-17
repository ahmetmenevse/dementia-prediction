dataInfoUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          h3('Dataset Description'),
          p("The dataset, sourced from a mobile healthcare service operated by non-governmental organizations managing elderly care centers in Hong Kong between 2008-2018, contains various demographic, health, and quality of life information for elderly individuals. 
          It includes 2299 observations and 12 variables, covering age, gender, height, weight, education level, financial status, and nutrition assessment scores."),
          hr(),
          p("Note: This dataset is a subset of the original data used in a comprehensive study. The original research included more variables and employed different methods to analyze the data. 
            For detailed information and methodologies used in the original study, please refer to the published research article. ", tags$a(href = "https://medinform.jmir.org/2020/8/e19870/", "More Info")),
          hr(),
          h4('Variables'),
          tags$ul(
            tags$li(tags$b("Age:"), " Age of individuals between 51-104."),
            tags$li(tags$b("Gender:"), " Gender of the individuals. (0: Female, 1: Male)."),
            tags$li(tags$b("Body_Height:"), " Individuals’ height."),
            tags$li(tags$b("Body_Weight:"), " Individuals’ weight."),
            tags$li(tags$b("Education_ID:"), " Education level of individuals. Categories: Non-educated, primary school, secondary school, tertiary."),
            tags$li(tags$b("Financial_status:"), " Financial status of individuals."),
            tags$li(tags$b("GDS:"), " Geriatric Depression Scale of the individuals (0-15). ", tags$a(href = "https://en.wikipedia.org/wiki/Geriatric_Depression_Scale", "More Info")),
            tags$li(tags$b("Independent_or_depend_on_family:"), " Either the individual is independent or dependent on the family."),
            tags$li(tags$b("Marital_Status_ID:"), " Marital status of the individual."),
            tags$li(tags$b("MNAa_total:"), " Mini Nutritional Assessment part A ranges between 0-14. ", tags$a(href = "https://pubmed.ncbi.nlm.nih.gov/9990575/", "More Info")),
            tags$li(tags$b("MNAb_total:"), " Mini Nutritional Assessment part B ranges between 0-16. ", tags$a(href = "https://pubmed.ncbi.nlm.nih.gov/24866345/", "More Info")),
            tags$li(tags$b("MMSE_class_binary:"), " Mini Mental State Exam (MMSE) class is the outcome label and indicates potential dementia risk (0: no dementia risk, 1: possible dementia risk).")
          ),
          h4('Outcome Label:'),
          tags$li(tags$b('MMSE_class_binary')),
          h4('Categorical Variables:'),
          tags$ul(
            lapply(categorical_vars, function(var) {
              tags$li(tags$b(var))
            })
          ),
          hr(),
          h4('Continuous Variables:'),
          tags$ul(
            lapply(continuous_vars, function(var) {
              tags$li(tags$b(var))
            })
          ),
          hr(),
          p("For next step: Please proceed to the 'Data Table' tab above to explore the dataset in detail.")
        ),
        mainPanel(
          h3('Variable Summaries'),
          fluidRow(uiOutput(ns('all_variable_summaries')))
        )
      )
    )
  )
}

dataInfoServer <- function(id, data, categorical_vars, continuous_vars, category_labels) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$all_variable_summaries <- renderUI({
      data_df <- data()
      
      all_summaries <- lapply(c(categorical_vars, continuous_vars), function(var) {
        if (var %in% categorical_vars) {
          summary_stats <- as.data.frame(table(data_df[[var]], useNA = "ifany"))
          colnames(summary_stats) <- c("Category", "Frequency")
          
          if (var %in% names(category_labels)) {
            summary_stats$Category <- category_labels[[var]][as.character(summary_stats$Category)]
          }
          column(4, tagList(
            h4(paste('Variable:', var)),
            tableOutput(ns(paste0('summary_', var))),
            renderTable({summary_stats})
          ))
        } else {
          summary_stats <- summary(data_df[[var]])
          summary_stats <- data.frame(Feature = names(summary_stats), Value = as.character(summary_stats))
          column(4, tagList(
            h4(paste('Variable:', var)),
            tableOutput(ns(paste0('summary_', var))),
            renderTable({summary_stats})
          ))
        }
      })
      do.call(fluidRow, all_summaries)
    })
  })
}
