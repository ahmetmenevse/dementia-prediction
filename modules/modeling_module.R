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
        actionButton(ns("run_models"), "Run Models")
      ),
      mainPanel(
        h4("Model Training Results"),
        verbatimTextOutput(ns("model_results")),
        h4("Model Comparison Results"),
        tableOutput(ns("comparison_table")),
        plotOutput(ns("roc_curve")),
        h4("Confusion Matrices"),
        uiOutput(ns("confusion_matrices"))
      )
    )
  )
}

# Server function for the Modeling Module
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
      comparison_table <- data.frame(Model = character(), AUC = numeric(), Accuracy = numeric(), Precision = numeric(), Recall = numeric(), F1 = numeric(), Specificity = numeric(), Kappa = numeric(), stringsAsFactors = FALSE)
      
      if ('Logistic Regression' %in% input$model_selection) {
        model <- glm(formula, data = train_data, family = binomial)
        predictions <- predict(model, newdata = test_data, type = "response")
        predicted_classes <- factor(ifelse(predictions > 0.5, 1, 0), levels = levels(factor(test_data$MMSE_class_binary)))
        roc_auc <- pROC::roc(test_data$MMSE_class_binary, predictions)
        models[['Logistic Regression']] <- list(model = model, auc = roc_auc$auc)
        rocs[['Logistic Regression']] <- roc_auc
        
        cm <- confusionMatrix(predicted_classes, factor(test_data$MMSE_class_binary))
        confusion_matrices[['Logistic Regression']] <- cm$table
        comparison_table <- rbind(comparison_table, data.frame(
          Model = "Logistic Regression",
          AUC = roc_auc$auc,
          Accuracy = cm$overall['Accuracy'],
          Precision = cm$byClass['Pos Pred Value'],
          Recall = cm$byClass['Sensitivity'],
          F1 = 2 * (cm$byClass['Pos Pred Value'] * cm$byClass['Sensitivity']) / (cm$byClass['Pos Pred Value'] + cm$byClass['Sensitivity']),
          Specificity = cm$byClass['Specificity'],
          Kappa = cm$overall['Kappa']
        ))
      }
      if ('Random Forest' %in% input$model_selection) {
        train_data$MMSE_class_binary <- as.factor(train_data$MMSE_class_binary)
        model <- randomForest(formula, data = train_data, importance = TRUE)
        predictions <- predict(model, newdata = test_data, type = "prob")[,2]
        predicted_classes <- factor(predict(model, newdata = test_data), levels = levels(factor(test_data$MMSE_class_binary)))
        roc_auc <- pROC::roc(test_data$MMSE_class_binary, predictions)
        models[['Random Forest']] <- list(model = model, auc = roc_auc$auc)
        rocs[['Random Forest']] <- roc_auc
        
        cm <- confusionMatrix(predicted_classes, factor(test_data$MMSE_class_binary))
        confusion_matrices[['Random Forest']] <- cm$table
        comparison_table <- rbind(comparison_table, data.frame(
          Model = "Random Forest",
          AUC = roc_auc$auc,
          Accuracy = cm$overall['Accuracy'],
          Precision = cm$byClass['Pos Pred Value'],
          Recall = cm$byClass['Sensitivity'],
          F1 = 2 * (cm$byClass['Pos Pred Value'] * cm$byClass['Sensitivity']) / (cm$byClass['Pos Pred Value'] + cm$byClass['Sensitivity']),
          Specificity = cm$byClass['Specificity'],
          Kappa = cm$overall['Kappa']
        ))
      }
      if ('SVM' %in% input$model_selection) {
        train_data$MMSE_class_binary <- as.factor(train_data$MMSE_class_binary)
        model <- e1071::svm(formula, data = train_data, probability = TRUE)
        predictions <- predict(model, newdata = test_data, probability = TRUE)
        probabilities <- attr(predictions, "probabilities")[,2]
        predicted_classes <- factor(ifelse(probabilities > 0.5, 1, 0), levels = levels(factor(test_data$MMSE_class_binary)))
        if (length(unique(test_data$MMSE_class_binary)) == 2 && !any(is.na(probabilities))) {
          roc_auc <- pROC::roc(test_data$MMSE_class_binary, probabilities)
          models[['SVM']] <- list(model = model, auc = roc_auc$auc)
          rocs[['SVM']] <- roc_auc
          
          cm <- confusionMatrix(predicted_classes, factor(test_data$MMSE_class_binary))
          confusion_matrices[['SVM']] <- cm$table
          comparison_table <- rbind(comparison_table, data.frame(
            Model = "SVM",
            AUC = roc_auc$auc,
            Accuracy = cm$overall['Accuracy'],
            Precision = cm$byClass['Pos Pred Value'],
            Recall = cm$byClass['Sensitivity'],
            F1 = 2 * (cm$byClass['Pos Pred Value'] * cm$byClass['Sensitivity']) / (cm$byClass['Pos Pred Value'] + cm$byClass['Sensitivity']),
            Specificity = cm$byClass['Specificity'],
            Kappa = cm$overall['Kappa']
          ))
        } else {
          showNotification("Not enough classes in test data for ROC calculation", type = "error")
          return(NULL)
        }
      }
      
      output$model_results <- renderPrint({
        lapply(models, function(x) summary(x$model))
      })
      
      output$comparison_table <- renderTable({
        comparison_table
      })
      
      output$roc_curve <- renderPlot({
        ggroc(rocs, aes = c("color")) + 
          ggtitle("ROC Curve Comparison") +
          theme_minimal() +
          scale_color_manual(values = c("Logistic Regression" = "red", "Random Forest" = "blue", "SVM" = "green"))
      })
      
      output$confusion_matrices <- renderUI({
        lapply(names(confusion_matrices), function(model) {
          tagList(
            h4(paste("Confusion Matrix:", model)),
            tableOutput(ns(paste0("conf_matrix_", model)))
          )
        })
      })
      
      lapply(names(confusion_matrices), function(model) {
        output[[paste0("conf_matrix_", model)]] <- renderTable({
          confusion_matrices[[model]]
        }, rownames = TRUE)
      })
    })
  })
}
