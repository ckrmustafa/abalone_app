library(shiny)
library(shinydashboard)
library(caret)
library(corrplot)
library(ggplot2)
library(randomForest)
library(DT)
library(dplyr)
library(pROC)
library(nnet)
library(xgboost)
library(e1071)
library(klaR)
library(gbm)

# UI Component
ui <- dashboardPage(
  dashboardHeader(title = "Abalone Data Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Overview", tabName = "data", icon = icon("database")),
      menuItem("EDA", tabName = "eda", icon = icon("chart-line")),
      menuItem("Model Training", tabName = "model", icon = icon("robot")),
      menuItem("Results", tabName = "results", icon = icon("trophy")),
      menuItem("Detailed Analysis", tabName = "analysis", icon = icon("search")),
      menuItem("Custom Predictions", tabName = "predict", icon = icon("bullseye"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "data",
              h2("Dataset Overview"),
              DTOutput("rawData")),
      tabItem(tabName = "eda",
              h2("Exploratory Data Analysis"),
              fluidRow(
                box(title = textOutput("edaBoxTitle"), plotOutput("corrPlot"), width = 8),
                box(title = "Controls", selectInput("edaType", "Visualization Type:", choices = c("Correlation" = "corr", "Feature Importance" = "imp")), width = 4)
              )),
      tabItem(tabName = "model",
              h2("Machine Learning Models"),
              fluidRow(
                box(title = "Model Configuration", width = 4,
                    selectizeInput("models", "Select Models:", multiple = TRUE,
                                   choices = c("Random Forest" = "rf", "XGBoost" = "xgb", "SVM" = "svm", "Logistic Regression" = "lr", "K-NN" = "knn", "Naive Bayes" = "nb", "GBM" = "gbm")),
                    numericInput("cvFolds", "Cross-Validation Folds:", 10),
                    numericInput("trainRatio", "Train-Test Split Ratio (0-1):", 0.8, min = 0.5, max = 0.9),
                    numericInput("seed", "Random Seed:", 42),
                    actionButton("trainBtn", "Train Models", icon = icon("play"))),
                box(title = "Training Progress", width = 8,
                    verbatimTextOutput("trainingStatus"),
                    plotOutput("modelProgress"))
              )),
      tabItem(tabName = "results",
              h2("Model Performance"),
              fluidRow(
                box(title = "Train Accuracy Comparison", width = 6, plotOutput("trainComparison")),
                box(title = "Test Metrics Comparison", width = 6, plotOutput("testComparison"))
              ),
              fluidRow(
                box(title = "Best Model", width = 4, verbatimTextOutput("bestModelInfo")),
                box(title = "Training Time Comparison", width = 8, plotOutput("trainingTimeComparison"))
              )
      ),
      tabItem(tabName = "analysis",
              h2("Detailed Model Analysis"),
              fluidRow(
                box(title = "Select Model", width = 12, selectInput("selectedModel", "Choose Model for Graphs:", choices = NULL)),
                box(title = "Confusion Matrix", width = 6, plotOutput("confMatrixPlot")),
                box(title = "ROC Curve", width = 6, plotOutput("rocPlot"))
              ),
              fluidRow(box(title = "Detailed Metrics", width = 12, DTOutput("detailedMetrics"))),
              fluidRow(box(title = "Misclassified Samples", width = 12, DTOutput("misclassifiedSamples")))
      ),
      tabItem(tabName = "predict",
              h2("Custom Predictions"),
              fluidRow(
                box(title = "Prediction Input", width = 6,
                    numericInput("length", "Length:", value = 0.5),
                    numericInput("diameter", "Diameter:", value = 0.4),
                    numericInput("height", "Height:", value = 0.1),
                    numericInput("whole_weight", "Whole Weight:", value = 0.9),
                    numericInput("shucked_weight", "Shucked Weight:", value = 0.3),
                    numericInput("viscera_weight", "Viscera Weight:", value = 0.2),
                    numericInput("shell_weight", "Shell Weight:", value = 0.1),
                    selectInput("sex", "Sex:", choices = c("M" = 1, "F" = 2, "I" = 3)),
                    actionButton("predictBtn", "Predict")),
                box(title = "Prediction Result", width = 6, verbatimTextOutput("customPrediction"))
              )
      )
    )
  )
)

# The server part continues from the user's current implementation.



server <- function(input, output, session) {
  # Dinamik başlık için renderText fonksiyonu
  output$edaBoxTitle <- renderText({
    if (input$edaType == "corr") {
      "Correlation Matrix"
    } else {
      "Feature Importance"
    }
  })
  
  # Abalone Dataset Preprocessing
  abalone <- reactive({
    df <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data",
                   header = FALSE,
                   col.names = c("Sex", "Length", "Diameter", "Height", 
                                 "Whole_weight", "Shucked_weight", 
                                 "Viscera_weight", "Shell_weight", "Rings"))
    
    df %>%
      mutate(
        Type = cut(Rings,
                   breaks = quantile(Rings, probs = c(0, 0.3, 0.7, 1), na.rm = TRUE),
                   labels = c("Young", "Adult", "Old"),
                   include.lowest = TRUE),
        Sex = as.numeric(factor(Sex, levels = c("M", "F", "I")))
      ) %>%
      dplyr::select(-Rings) %>%
      na.omit()
  })
  
  output$corrPlot <- renderPlot({
    if (input$edaType == "corr") {
      cor_data <- trainTestSplit()$train %>% dplyr::select(-Type)
      if (ncol(cor_data) > 1) {  # Sütun sayısını kontrol edin
        corrplot(cor(cor_data), 
                 method = "color", 
                 type = "upper", 
                 tl.col = "black", 
                 title = "Correlation Matrix", 
                 mar = c(0, 0, 1, 0), 
                 addCoef.col = "black", 
                 number.cex = 0.7, 
                 col = colorRampPalette(c("blue", "white", "red"))(200))
      } else {
        plot.new()
        title("Insufficient Data for Correlation")
      }
    } else {
      # Feature Importance için aşağıdaki kod çalışır
      rf <- randomForest(Type ~ ., data = trainTestSplit()$train, importance = TRUE)
      imp <- importance(rf, type = 1)
      ggplot(data.frame(Feature = rownames(imp), Importance = imp[,1]), 
             aes(x = reorder(Feature, Importance), y = Importance)) +
        geom_col(fill = "steelblue") +
        coord_flip() +
        theme_minimal(base_size = 14) +
        labs(title = "Feature Importance", 
             x = "Features", 
             y = "Importance")
    }
  })
  # Train-Test Split
  trainTestSplit <- reactive({
    set.seed(input$seed)
    df <- abalone()
    trainIndex <- createDataPartition(df$Type, p = input$trainRatio, list = FALSE)
    list(
      train = df[trainIndex, ],
      test = df[-trainIndex, ]
    )
  })
  
  observeEvent(modelResults(), {
    updateSelectInput(session, "selectedModel",
                      choices = names(modelResults()),
                      selected = names(modelResults())[1])
  })
  
  # Model Training
  modelResults <- eventReactive(input$trainBtn, {
    req(input$models)
    
    model_list <- list(
      rf = list(method = "rf", tuneGrid = data.frame(mtry = c(2, 4, 6))),
      xgb = list(method = "xgbTree", 
                 tuneGrid = expand.grid(nrounds = 100,
                                        max_depth = 3:6,
                                        eta = c(0.01, 0.1),
                                        gamma = 0,
                                        colsample_bytree = 1,
                                        min_child_weight = 1,
                                        subsample = 1)),
      svm = list(method = "svmRadial", 
                 tuneGrid = expand.grid(C = 10^(-1:1),
                                        sigma = 10^(-2:0))),
      lr = list(method = "multinom"),
      knn = list(method = "knn", 
                 tuneGrid = data.frame(k = seq(5, 25, by = 5))),
      nb = list(method = "naive_bayes", 
                tuneGrid = expand.grid(laplace = c(0, 1),
                                       usekernel = c(TRUE, FALSE),
                                       adjust = c(0.5, 1))),
      gbm = list(method = "gbm", 
                 tuneGrid = expand.grid(interaction.depth = c(1, 3),
                                        n.trees = c(50, 100),
                                        shrinkage = c(0.1, 0.01),
                                        n.minobsinnode = 10))
    )
    
    train_control <- trainControl(method = "cv",
                                  number = input$cvFolds,
                                  classProbs = TRUE,
                                  savePredictions = "final")
    
    withProgress(message = "Training Models", value = 0, {
      results <- list()
      selected_models <- model_list[input$models]
      
      for (i in seq_along(selected_models)) {
        incProgress(1/length(selected_models), 
                    detail = names(selected_models)[i])
        
        results[[names(selected_models)[i]]] <- caret::train(
          Type ~ .,
          data = trainTestSplit()$train,
          method = selected_models[[i]]$method,
          trControl = train_control,
          tuneGrid = selected_models[[i]]$tuneGrid
        )
      }
      results
    })
  })
  
  # Train Accuracy and Kappa Comparison
  output$trainComparison <- renderPlot({
    req(modelResults())  # Eğitim sonuçlarının mevcut olup olmadığını kontrol et
    
    resamps <- resamples(modelResults())
    bwplot(resamps, metric = c("Accuracy", "Kappa"), 
           main = "Training Set Metrics Comparison",
           layout = c(2, 1), 
           par.settings = list(fontsize = list(text = 12)))  # Yazı boyutu
  })
  
  # Test Accuracy and Kappa Comparison
  output$testComparison <- renderPlot({
    req(modelResults())  # Model sonuçlarının mevcut olup olmadığını kontrol et
    
    test_metrics <- lapply(modelResults(), function(model) {
      test_predictions <- predict(model, trainTestSplit()$test)
      conf_mat <- confusionMatrix(test_predictions, trainTestSplit()$test$Type)
      list(
        Accuracy = as.numeric(conf_mat$overall["Accuracy"]),
        Kappa = as.numeric(conf_mat$overall["Kappa"])
      )
    })
    
    test_df <- do.call(rbind, lapply(test_metrics, as.data.frame))
    rownames(test_df) <- names(modelResults())  # Model isimlerini satır isimleri olarak ekle
    
    barplot(t(test_df), beside = TRUE, 
            col = c("steelblue", "orange"), 
            ylim = c(0, 1), 
            names.arg = rownames(test_df), 
            legend.text = c("Accuracy", "Kappa"), 
            args.legend = list(x = "topright", cex = 0.9), 
            main = "Test Set Metrics Comparison")
  })
  
  # Best Model Information
  output$bestModelInfo <- renderPrint({
    req(modelResults())  # Model sonuçlarının mevcut olduğunu kontrol et
    
    # Accuracy (Doğruluk) bazlı en iyi modeli seç
    best_model <- which.max(sapply(modelResults(), function(model) {
      max(model$results$Accuracy, na.rm = TRUE)
    }))
    
    best_model_name <- names(modelResults())[best_model]
    
    cat("Best Model Based on Accuracy:\n")
    cat(best_model_name, "\n")
    
    # En iyi modelin hiperparametrelerini göster
    cat("\nOptimal Hyperparameters:\n")
    print(modelResults()[[best_model]]$bestTune)
  })
  # Training Time Comparison
  output$trainingTimeComparison <- renderPlot({
    req(modelResults())  # Model sonuçlarının mevcut olup olmadığını kontrol et
    
    # Eğitim sürelerini çekme ve görselleştirme
    training_times <- sapply(modelResults(), function(model) {
      model$times$everything["elapsed"]
    })
    
    barplot(training_times, 
            col = "lightgreen", 
            names.arg = names(modelResults()), 
            main = "Model Training Time Comparison", 
            xlab = "Models", 
            ylab = "Training Time (seconds)")
  })
  
  # Confusion Matrix Visualization
  output$confMatrixPlot <- renderPlot({
    req(modelResults(), input$selectedModel)  # Gereklilik kontrolü
    selected_model <- input$selectedModel
    test_predictions <- predict(modelResults()[[selected_model]], trainTestSplit()$test)
    conf_mat <- confusionMatrix(test_predictions, trainTestSplit()$test$Type)
    df <- as.data.frame(conf_mat$table)
    ggplot(data = df, aes(x = Reference, y = Prediction, fill = Freq)) +
      geom_tile(color = "black") +
      geom_text(aes(label = Freq), color = "white", size = 6) +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      theme_minimal(base_size = 15) +
      labs(title = paste("Confusion Matrix:", selected_model),
           x = "True Labels",
           y = "Predicted Labels")
  })
  
  # ROC Curve Plot
  output$rocPlot <- renderPlot({
    req(modelResults(), input$selectedModel)
    selected_model <- input$selectedModel
    test_set <- trainTestSplit()$test
    probs <- predict(modelResults()[[selected_model]], test_set, type = "prob")
    
    roc_curves <- lapply(levels(test_set$Type), function(class) {
      roc(response = ifelse(test_set$Type == class, 1, 0), predictor = probs[, class])
    })
    
    plot(roc_curves[[1]], main = paste("ROC Curve:", selected_model), col = 1, lwd = 3, cex.main = 1.5)
    auc_values <- numeric(length(roc_curves))
    
    for (i in seq_along(roc_curves)) {
      if (i > 1) {
        plot(roc_curves[[i]], add = TRUE, col = i, lwd = 3)
      }
      auc_values[i] <- auc(roc_curves[[i]])
    }
    
    legend("bottomright", 
           legend = paste(levels(test_set$Type), ": AUC =", round(auc_values, 4)),
           col = 1:length(roc_curves), 
           lty = 1, 
           lwd = 3, 
           cex = 1.2, 
           bty = "n")
  })
  
  # Training Status with Full Hyperparameter Ranges and Optimal Values
  output$trainingStatus <- renderPrint({
    req(modelResults())  # Model sonuçlarının mevcut olup olmadığını kontrol et
    
    cat("Hyperparameter Ranges and Optimal Values for Models:\n")
    lapply(names(modelResults()), function(model_name) {
      model <- modelResults()[[model_name]]
      cat("\nModel:", model_name, "\n")
      cat("Hyperparameter Ranges:\n")
      
      # Hiperparametre aralıkları
      if (!is.null(model$control$tuneGrid)) {
        print(model$control$tuneGrid)  # Tüm aralıkları yazdır
      } else {
        cat("No specific ranges provided.\n")
      }
      
      # Optimum hiperparametreler
      cat("\nOptimal Hyperparameters:\n")
      print(model$bestTune)
    })
  })
  
  # Detailed Metrics Table
  output$detailedMetrics <- renderDT({
    req(modelResults())
    test_metrics <- lapply(modelResults(), function(model) {
      test_predictions <- predict(model, trainTestSplit()$test)
      conf_mat <- confusionMatrix(test_predictions, trainTestSplit()$test$Type)
      list(
        Accuracy = round(as.numeric(conf_mat$overall["Accuracy"]), 4),
        Kappa = round(as.numeric(conf_mat$overall["Kappa"]), 4),
        Precision = round(mean(conf_mat$byClass[, "Precision"], na.rm = TRUE), 4),
        Recall = round(mean(conf_mat$byClass[, "Recall"], na.rm = TRUE), 4),
        F1 = round(mean(conf_mat$byClass[, "F1"], na.rm = TRUE), 4)
      )
    })
    metrics <- do.call(rbind, lapply(test_metrics, function(x) as.data.frame(x)))
    rownames(metrics) <- names(modelResults())
    datatable(metrics, options = list(scrollX = TRUE, pageLength = 5))
  })
  
  # Misclassified Samples
  output$misclassifiedSamples <- renderDT({
    req(modelResults(), input$selectedModel)  # Gereklilik kontrolü
    selected_model <- input$selectedModel
    test_set <- trainTestSplit()$test
    test_predictions <- predict(modelResults()[[selected_model]], test_set)
    misclassified <- test_set[test_set$Type != test_predictions, ]
    datatable(misclassified, options = list(scrollX = TRUE, pageLength = 5))
  })
  
  # Render Raw Data Table
  output$rawData <- renderDT({
    datatable(abalone(), options = list(scrollX = TRUE, pageLength = 5))
  })
  
  observeEvent(input$predictBtn, {
    req(modelResults())
    
    # En yüksek doğruluğa sahip modeli seç
    best_model_index <- which.max(sapply(modelResults(), function(m) max(m$results$Accuracy, na.rm = TRUE)))
    best_model <- modelResults()[[best_model_index]]
    
    # Yeni tahmin verisini oluştur
    new_data <- data.frame(
      Sex = as.numeric(input$sex),
      Length = input$length,
      Diameter = input$diameter,
      Height = input$height,
      Whole_weight = input$whole_weight,
      Shucked_weight = input$shucked_weight,
      Viscera_weight = input$viscera_weight,
      Shell_weight = input$shell_weight
    )
    
    # Tahmini yap
    prediction <- predict(best_model, new_data)
    
    # Tahmin sonucunu kullanıcıya göster
    output$customPrediction <- renderPrint({
      paste("Predicted Age Class:", as.character(prediction))
    })
  })
  
}

# Shiny App
shinyApp(ui = ui, server = server)