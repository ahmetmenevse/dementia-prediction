edaUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          h3("EDA Module"),
          actionButton(ns("correlation_btn"), "Calculate Correlation"),
          br(),
          h4("Scaling and Partitioning"),
          h5("Scaling Methods"),
          p(strong("None:"), " No scaling will be applied."),
          p(strong("Standardization:"), " Centers the data by subtracting the mean and scales by dividing by the standard deviation."),
          p(strong("Normalization:"), " Scales the data to the range [0, 1] by subtracting the minimum value and dividing by the range."),
          radioButtons(ns('scaling_method'), 'Select Scaling Method', choices = c('None', 'Standardization', 'Normalization')),
          actionButton(ns("scale_btn"), "Scale Data"),
          verbatimTextOutput(ns("scaling_results")),
          br(),
          h5("Partitioning"),
          p("Splits the dataset into training and testing sets based on the selected ratio."),
          sliderInput(ns("train_split"), "Train/Test Split Ratio", min = 0.5, max = 0.9, value = 0.7, step = 0.01),
          actionButton(ns("partition_btn"), "Partition Data"),
          verbatimTextOutput(ns("partition_results")),
          hr(),
          actionButton(ns("proceed_btn"), "Proceed to Modeling"),
          br(),
          uiOutput(ns("next_steps"))
        ),
        mainPanel(
          h4("Correlation Table"),
          tableOutput(ns("correlation_table")),
          br(),
          h4("Variable Relationships with MMSE_class_binary"),
          plotOutput(ns("plots"), height = "1800px"),
          br()
        )
      )
    )
  )
}

edaServer <- function(id, data, category_labels) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    correlation_data <- reactiveVal(NULL)
    scaled_data <- reactiveVal(NULL)
    partitioned_data <- reactiveVal(NULL)
    
    observeEvent(input$correlation_btn, {
      df <- data()
      if (!is.null(df)) {
        numeric_vars <- sapply(df, is.numeric)
        df_numeric <- df[, numeric_vars]
        df_numeric <- df_numeric[, !colnames(df_numeric) %in% "X"]
        
        corr_matrix <- cor(df_numeric, method = "spearman", use = "complete.obs")
        correlation_data(corr_matrix)
        output$correlation_table <- renderTable({
          corr_data <- correlation_data()
          corr_data <- cbind(Variable = rownames(corr_data), round(corr_data, 3))
          rownames(corr_data) <- NULL
          corr_data
        }, rownames = FALSE)
        
        output$plots <- renderPlot({
          plots <- list()
          for (var in colnames(df)) {
            if (var != "MMSE_class_binary" && var != "X") {
              if (var %in% categorical_vars) {
                if (length(unique(df[[var]])) <= 2) {
                  p <- ggplot(df, aes(x = factor(MMSE_class_binary), fill = factor(.data[[var]]))) + 
                    geom_bar(position = "dodge") + 
                    theme_minimal() + 
                    ggtitle(paste(var, "vs MMSE_class_binary")) +
                    theme(legend.position = "bottom") +
                    scale_fill_manual(values = c("0" = "pink", "1" = "lightblue"), 
                                      labels = category_labels[[var]])
                } else {
                  p <- ggplot(df, aes(x = .data[[var]], fill = factor(MMSE_class_binary))) + 
                    geom_density(alpha = 0.5) + 
                    theme_minimal() + 
                    ggtitle(paste(var, "vs MMSE_class_binary")) +
                    theme(legend.position = "bottom")
                }
              } else {
                p <- ggplot(df, aes(x = factor(MMSE_class_binary), y = .data[[var]], fill = factor(MMSE_class_binary))) + 
                  geom_boxplot(outlier.size = 0.5) + 
                  theme_minimal() + 
                  ggtitle(paste(var, "vs MMSE_class_binary")) + 
                  theme(legend.position = "bottom")
              }
              plots[[var]] <- p
            }
          }
          gridExtra::grid.arrange(grobs = plots, ncol = 3)
        })
      }
    })
    
    observeEvent(input$scale_btn, {
      df <- data()
      if (!is.null(df)) {
        if (input$scaling_method == 'Standardization') {
          df[, continuous_vars] <- scale(df[, continuous_vars])
        } else if (input$scaling_method == 'Normalization') {
          df[, continuous_vars] <- lapply(df[, continuous_vars], function(x) (x - min(x)) / (max(x) - min(x)))
        }
        scaled_data(df)
        output$scaling_results <- renderPrint({
          summary(df[, continuous_vars])
        })
      }
    })
    
    observeEvent(input$partition_btn, {
      df <- scaled_data()
      if (is.null(df)) {
        df <- data()
      }
      set.seed(1)  # For reproducibility
      train_index <- sample(seq_len(nrow(df)), size = input$train_split * nrow(df))
      train_data <- df[train_index, ]
      test_data <- df[-train_index, ]
      partitioned_data(list(train = train_data, test = test_data))
      output$partition_results <- renderPrint({
        list(Train = nrow(train_data), Test = nrow(test_data))
      })
    })
    
    observeEvent(input$proceed_btn, {
      if (!is.null(partitioned_data())) {
        output$next_steps <- renderUI({
          tagList(
            h4("Proceed to Modeling"),
            p("You can now proceed to the modeling phase with the partitioned and scaled data.")
          )
        })
      }
    })
    
    return(partitioned_data)  # Return the partitioned data for use in modeling
  })
}