data = read.csv('dataset ICT583 2024S1.csv')



####Summary Statistic####
library(tidyverse)
str(data)
head(data)
names(data[,-1])
continuous_vars <- c("Age", "Body_Height", "Body_Weight", "GDS", "MNAa_total", "MNAb_total", "Education_ID", "Financial_status")
categorical_vars <- c("MMSE_class_binary", "Gender", "Independent_or_depend_on_family", "Marital_status_ID")

data %>% select(all_of(categorical_vars)) %>% map(table)

data %>% select(all_of( continuous_vars)) %>%
  map(., ~{
    c(
      Mean = mean(.x, na.rm = T) %>% round(2),
      SD = sd(.x, na.rm = T)%>% round(2)
    )
  }
  )

sum = summary(data[continuous_vars])
sum

#Imputate KNN to missing values
library(DMwR2)
mising_values = colSums(is.na(data[,-1]))
mising_values%>% write.csv("missvalues.csv")
mising_values

knn_imputation <- knnImputation(data, k = 5)
str(knn_imputation[,-1])
colSums(is.na(knn_imputation[,-1]))

#Correlation
corr = knn_imputation[,-1] %>%  select_if(is.numeric) %>% 
  cor(., method = 'spearman') %>% round(3)
corr %>% write.csv("corr.csv")
corr


####Visualisation####
library(cowplot)
age <- ggplot(knn_imputation, aes(x = factor(MMSE_class_binary), y = Age, fill = factor(MMSE_class_binary))) +
  theme(legend.position = "bottom") +
  geom_boxplot()

edu <- ggplot(knn_imputation, aes(x = Education_ID, fill = factor(MMSE_class_binary))) +
  theme(legend.position = "bottom") +
  geom_density(alpha = .5)

mnab <- ggplot(knn_imputation, aes(x = factor(MMSE_class_binary), y = MNAb_total, fill = factor(MMSE_class_binary))) +
  theme(legend.position = "bottom") +
  geom_boxplot()
height <- ggplot(knn_imputation, aes(x = factor(MMSE_class_binary), y = Body_Height, fill = factor(MMSE_class_binary))) +
  theme(legend.position = "bottom") +
  geom_boxplot()

plot_grid(age, edu, mnab, height, ncol = 2, nrow = 2)


#Scaling
data2 = knn_imputation %>%  mutate_if(~ n_distinct(.) > 2, scale)
str(data2[,-1])


#Partitioning
library(caret)

set.seed(1)

id <- createDataPartition(data2 $MMSE_class_binary, p =.7, list = F)

traning <- data2[id,]
testing <- data2[-id,]

dim(traning)
dim(testing)

#####Logistic Regression ######
#building model
formula1 = MMSE_class_binary ~ Age + Education_ID + MNAb_total + Body_Height 
LGModel <- glm(formula1, binomial, traning)
sum = summary(LGModel)
sum

log_coefficient = sum $ coefficients %>% as.data.frame() %>% 
  mutate(odds_ratio = exp(Estimate)) %>% round(3)


#estimate probability
estimate_prob <- predict(LGModel, testing, type="response")
str(estimate_prob)


# binary prediction 
log_pred <- round(estimate_prob)


#confussion matrix
log_performance = table(log_pred, testing$MMSE_class_binary) %>% 
  confusionMatrix(., positive = '1')
log_performance

#pROC
library(pROC)
log_roc = roc(testing$MMSE_class_binary, estimate_prob)
log_roc

# Roc 
library(ROCR)
pred <- prediction(log_pred, testing$MMSE_class_binary)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=FALSE)


####Support Vector Machine#####
library(e1071)
#building model
svm_model <- svm(formula = formula1, data = traning, type = "C-classification", probability = TRUE)
summary(svm_model)

svm_predic = predict(svm_model, testing)
str(svm_predic)

#estimate probability
svm_prob = predict(svm_model, testing, probability = T) %>% 
  attr("probabilities") %>% .[,1]
str(svm_prob)

#confussion matrix
svm_perform = table(svm_predic, testing$MMSE_class_binary) %>% 
  confusionMatrix(., positive = "1")
svm_perform

#proc
svm_roc = roc(testing $ MMSE_class_binary, svm_prob)
svm_roc

####Result####
#performance
models_performance = cbind.data.frame(
  lr = c(log_performance$overall, log_performance$byClass, AUC = log_roc$auc),
  svm = c(svm_perform$overall, svm_perform$byClass, AUC = svm_roc$auc)
) %>% 
  round(3) %>% 
  mutate(best = apply(., 1, which.max))

models_performance %>% 
  write.csv("models_performance.csv")

models_performance

#Visualise model performance

ggroc(
  list(log_roc, svm_roc),
  legacy.axes = TRUE) +
  scale_color_discrete(
    labels = c(
      paste("Logistic regression, AUC", round(log_roc$auc, 3)),
      paste("Support vector machine, AUC", round(svm_roc$auc, 3))
    )
) +
labs(col = "Model") +
theme_classic() +
theme(legend.position = c(.8,.2))
