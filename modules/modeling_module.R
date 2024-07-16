library(shiny)
library(randomForest)
library(e1071)
library(pROC)
library(ggplot2)
library(caret)

# UI for the Modeling Module
modelingUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        uiOutput(ns("variable_selection")),
        uiOutput(ns("model_selection_ui")),
        actionButton(ns("run_models"), "Run Models"),
      ),
      mainPanel(
        h4("Model Training Results"),
        verbatimTextOutput(ns("model_results")),
        h4("Confusion Matrices"),
        uiOutput(ns("confusion_matrices")),
        fluidRow(
          column(4, uiOutput(ns("conf_matrix_lr"))),
          column(4, uiOutput(ns("conf_matrix_rf"))),
          column(4, uiOutput(ns("conf_matrix_svm")))
        ),
        h4("Model Comparison Results"),
        tableOutput(ns("comparison_table")),
        plotOutput(ns("roc_curve"))
      )
    )
  )
}

modelingServer <- function(id, data, category_labels, continuous_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$variable_selection <- renderUI({
      df <- data()$train
      if (!is.null(df)) {
        selected_vars <- setdiff(names(df), "MMSE_class_binary")
        checkboxGroupInput(ns('selected_vars'), 'Select Variables for Modeling', choices = selected_vars, selected = NULL)
      }
    })
    
    output$model_selection_ui <- renderUI({
      checkboxGroupInput(ns('model_selection'), 'Select Models', choices = c('Logistic Regression', 'Random Forest', 'SVM'))
    })
    
    output$model_choice_ui <- renderUI({
      selectInput(ns("selected_model"), "Select the Best Model", choices = c("Logistic Regression", "Random Forest", "SVM"))
    })
    
    observeEvent(input$run_models, {
      req(input$selected_vars, input$model_selection)
      
      df <- data()
      
      if (!"MMSE_class_binary" %in% colnames(df$train)) {
        showNotification("MMSE_class_binary not found in the dataset", type = "error")
        return(NULL)
      }
      
      train_data <- df$train
      test_data <- df$test
      
      formula <- as.formula(paste("MMSE_class_binary ~", paste(input$selected_vars, collapse = "+")))
      
      models <- list()
      rocs <- list()
      confusion_matrices <- list()
      comparison_table <- data.frame(Metric = c("AUC", "Accuracy", "Precision", "Recall", "F1", "Specificity", "Kappa"), Logistic_Regression = NA, Random_Forest = NA, SVM = NA)
      
      if ('Logistic Regression' %in% input$model_selection) {
        model <- glm(formula, data = train_data, family = binomial)
        predictions <- predict(model, newdata = test_data, type = "response")
        predicted_classes <- ifelse(predictions > 0.5, 1, 0)
        roc_auc <- pROC::roc(test_data$MMSE_class_binary, predictions)
        models[['Logistic Regression']] <- list(model = model, auc = roc_auc$auc)
        rocs[['Logistic Regression']] <- roc_auc
        
        cm <- confusionMatrix(factor(predicted_classes), factor(test_data$MMSE_class_binary), positive = '1')
        confusion_matrices[['Logistic Regression']] <- cm$table
        comparison_table$Logistic_Regression <- c(
          roc_auc$auc,
          cm$overall['Accuracy'],
          cm$byClass['Pos Pred Value'],
          cm$byClass['Sensitivity'],
          2 * (cm$byClass['Pos Pred Value'] * cm$byClass['Sensitivity']) / (cm$byClass['Pos Pred Value'] + cm$byClass['Sensitivity']),
          cm$byClass['Specificity'],
          cm$overall['Kappa']
        )
      }
      if ('Random Forest' %in% input$model_selection) {
        train_data$MMSE_class_binary <- as.factor(train_data$MMSE_class_binary)
        set.seed(1)
        model <- randomForest(formula, data = train_data, importance = TRUE)
        predictions <- predict(model, newdata = test_data, type = "prob")[,2]
        predicted_classes <- factor(ifelse(predictions > 0.5, 1, 0))
        roc_auc <- pROC::roc(test_data$MMSE_class_binary, predictions)
        models[['Random Forest']] <- list(model = model, auc = roc_auc$auc)
        rocs[['Random Forest']] <- roc_auc
        
        cm <- confusionMatrix(factor(predicted_classes), factor(test_data$MMSE_class_binary), positive = '1')
        confusion_matrices[['Random Forest']] <- cm$table
        comparison_table$Random_Forest <- c(
          roc_auc$auc,
          cm$overall['Accuracy'],
          cm$byClass['Pos Pred Value'],
          cm$byClass['Sensitivity'],
          2 * (cm$byClass['Pos Pred Value'] * cm$byClass['Sensitivity']) / (cm$byClass['Pos Pred Value'] + cm$byClass['Sensitivity']),
          cm$byClass['Specificity'],
          cm$overall['Kappa']
        )
      }
      if ('SVM' %in% input$model_selection) {
        train_data$MMSE_class_binary <- as.factor(train_data$MMSE_class_binary)
        model <- e1071::svm(formula, data = train_data, probability = TRUE)
        predictions <- predict(model, newdata = test_data, probability = TRUE)
        probabilities <- attr(predictions, "probabilities")[,2]
        predicted_classes <- ifelse(probabilities > 0.5, 1, 0)
        roc_auc <- pROC::roc(test_data$MMSE_class_binary, probabilities)
        models[['SVM']] <- list(model = model, auc = roc_auc$auc)
        rocs[['SVM']] <- roc_auc
        
        cm <- confusionMatrix(factor(predicted_classes), factor(test_data$MMSE_class_binary), positive = '1')
        confusion_matrices[['SVM']] <- cm$table
        comparison_table$SVM <- c(
          roc_auc$auc,
          cm$overall['Accuracy'],
          cm$byClass['Pos Pred Value'],
          cm$byClass['Sensitivity'],
          2 * (cm$byClass['Pos Pred Value'] * cm$byClass['Sensitivity']) / (cm$byClass['Pos Pred Value'] + cm$byClass['Sensitivity']),
          cm$byClass['Specificity'],
          cm$overall['Kappa']
        )
      }
      
      comparison_table$Best_Model <- apply(comparison_table[, -1], 1, function(row) {
        colnames(comparison_table)[-1][which.max(row)]
      })
      
      output$model_results <- renderPrint({
        lapply(models, function(x) summary(x$model))
      })
      
      output$comparison_table <- renderTable({
        comparison_table
      }, rownames = TRUE)
      
      output$roc_curve <- renderPlot({
        ggroc(rocs, aes = c("color")) + 
          ggtitle("ROC Curve Comparison") +
          theme_minimal() +
          scale_color_manual(values = c("Logistic Regression" = "red", "Random Forest" = "blue", "SVM" = "green"))
      })
      
      output$conf_matrix_lr <- renderUI({
        tagList(
          h4("Confusion Matrix: Logistic Regression"),
          tableOutput(ns("conf_matrix_lr_table"))
        )
      })
      
      output$conf_matrix_rf <- renderUI({
        tagList(
          h4("Confusion Matrix: Random Forest"),
          tableOutput(ns("conf_matrix_rf_table"))
        )
      })
      
      output$conf_matrix_svm <- renderUI({
        tagList(
          h4("Confusion Matrix: SVM"),
          tableOutput(ns("conf_matrix_svm_table"))
        )
      })
      
      output$conf_matrix_lr_table <- renderTable({
        cm <- confusion_matrices[['Logistic Regression']]
        cm <- as.data.frame.matrix(cm)
        colnames(cm) <- c("Predicted 0", "Predicted 1")
        rownames(cm) <- c("Actual 0", "Actual 1")
        cm
      }, rownames = TRUE)
      
      output$conf_matrix_rf_table <- renderTable({
        cm <- confusion_matrices[['Random Forest']]
        cm <- as.data.frame.matrix(cm)
        colnames(cm) <- c("Predicted 0", "Predicted 1")
        rownames(cm) <- c("Actual 0", "Actual 1")
        cm
      }, rownames = TRUE)
      
      output$conf_matrix_svm_table <- renderTable({
        cm <- confusion_matrices[['SVM']]
        cm <- as.data.frame.matrix(cm)
        colnames(cm) <- c("Predicted 0", "Predicted 1")
        rownames(cm) <- c("Actual 0", "Actual 1")
        cm
      }, rownames = TRUE)
    })
  })
}
