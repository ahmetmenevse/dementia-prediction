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
        uiOutput(ns("variable_selection_explanation")),
        uiOutput(ns("variable_selection")),
        hr(),
        uiOutput(ns("model_selection_explanation")),
        uiOutput(ns("model_selection_ui")),
        actionButton(ns("run_models"), "Run Models"),
      ),
      mainPanel(
        h4("Model Training Results"),
        verbatimTextOutput(ns("model_results")),
        uiOutput(ns("confusion_matrices_explanation")),
        uiOutput(ns("confusion_matrices")),
        fluidRow(
          column(4, uiOutput(ns("conf_matrix_lr"))),
          column(4, uiOutput(ns("conf_matrix_rf"))),
          column(4, uiOutput(ns("conf_matrix_svm")))
        ),
        uiOutput(ns("model_comparison_explanation")),
        tableOutput(ns("comparison_table")),
        plotOutput(ns("roc_curve"))
      )
    )
  )
}

modelingServer <- function(id, data, category_labels, continuous_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$variable_selection_explanation <- renderUI({
      tagList(
        h4("Variable Selection:"),
        p("Please select the variables you want to include in your model. The selection of variables has a significant impact on the accuracy and performance of your model."),
        p("Consider the correlation results calculated in previous steps to select variables that have a strong relationship with the target variable."),
        p("Use the correlation matrices and charts to understand the relationships between variables. Strong positive or negative correlations can help identify important variables."),
        p("By using this information, choose the most appropriate variables for your model to maximize its performance.")
      )
    })
    
    output$model_selection_explanation <- renderUI({
      tagList(
        h4("Machine Learning Methods:"),
        tags$ul(
          tags$li(tags$b(tags$i(tags$u("Logistic Regression:"))), " Commonly used for binary classification problems. It is a simple and interpretable model. It performs well when there is a linear relationship between the independent variables and the target variable."),
          tags$li(tags$b(tags$i(tags$u("Random Forest:"))), " A powerful and flexible classification method that combines many decision trees. It reduces the risk of overfitting and can handle complex datasets effectively."),
          tags$li(tags$b(tags$i(tags$u("SVM (Support Vector Machine):"))), " Used to find the best separation between classes. It can be effective in complex datasets."),
          tags$li("Consider the characteristics of your data and the strengths of each method when making your selection.")
        )
      )
    })
    
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
      
      format_confusion_matrix <- function(cm) {
        cm_df <- as.data.frame.matrix(cm)
        cm_df_formatted <- cm_df
        cm_df_formatted[1, 1] <- paste(cm_df[1, 1], "(TN)")
        cm_df_formatted[1, 2] <- paste(cm_df[1, 2], "(FP)")
        cm_df_formatted[2, 1] <- paste(cm_df[2, 1], "(FN)")
        cm_df_formatted[2, 2] <- paste(cm_df[2, 2], "(TP)")
        colnames(cm_df_formatted) <- c("Predicted 0", "Predicted 1")
        rownames(cm_df_formatted) <- c("Actual 0", "Actual 1")
        cm_df_formatted
      }
      
      output$conf_matrix_lr_table <- renderTable({
        format_confusion_matrix(confusion_matrices[['Logistic Regression']])
      }, rownames = TRUE)
      
      output$conf_matrix_rf_table <- renderTable({
        format_confusion_matrix(confusion_matrices[['Random Forest']])
      }, rownames = TRUE)
      
      output$conf_matrix_svm_table <- renderTable({
        format_confusion_matrix(confusion_matrices[['SVM']])
      }, rownames = TRUE)
      
      output$confusion_matrices_explanation <- renderUI({
        tagList(
          h4("Confusion Matrices:"),
          tags$ul(
            tags$li(tags$b(tags$i(tags$u("True Positives (TP):"))), " Correctly predicted positive cases."),
            tags$li(tags$b(tags$i(tags$u("True Negatives (TN):"))), " Correctly predicted negative cases."),
            tags$li(tags$b(tags$i(tags$u("False Positives (FP):"))), " Incorrectly predicted positive cases."),
            tags$li(tags$b(tags$i(tags$u("False Negatives (FN):"))), " Incorrectly predicted negative cases."),
            tags$li("Understanding the confusion matrix helps identify which classes are more often misclassified and improve your model accordingly.")
          )
        )
      })
      
      output$model_comparison_explanation <- renderUI({
        tagList(
          h4("Model Comparison Results:"),
          tags$ul(
            tags$li(tags$b(tags$i(tags$u("AUC (Area Under the ROC Curve):"))), " Evaluates the model's ability to classify correctly. The closer to 1, the better."),
            tags$li(tags$b(tags$i(tags$u("Accuracy:"))), " The proportion of correct predictions over the total predictions."),
            tags$li(tags$b(tags$i(tags$u("Precision:"))), " The proportion of correctly predicted positive cases out of all predicted positives. TP / (TP + FP)."),
            tags$li(tags$b(tags$i(tags$u("Recall (Sensitivity):"))), " The proportion of actual positives correctly predicted. TP / (TP + FN)."),
            tags$li(tags$b(tags$i(tags$u("F1 Score:"))), " The harmonic mean of precision and recall, balancing the two metrics."),
            tags$li(tags$b(tags$i(tags$u("Specificity:"))), " The proportion of actual negatives correctly predicted. TN / (TN + FP)."),
            tags$li(tags$b(tags$i(tags$u("Kappa:"))), " Evaluates the model's performance compared to random chance."),
            tags$li("Use these metrics to compare the models and determine the best performing one. High accuracy and AUC values are generally indicators of good performance. However, depending on your specific case, other metrics like recall or precision might be more important.")
          )
        )
      })
    })
  })
}
