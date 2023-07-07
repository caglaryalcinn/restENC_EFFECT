library(tidyverse) 
library(dplyr)
library(missForest)
library(missMethods)
library(naniar)
library(tidyr)
library(mice)
library(randomForest)
library(caret)
library(ggplot2)
library(tidyr)
library("writexl")
library(keras)
library(mlbench)
library(dplyr)
library(magrittr)
library(neuralnet)
library(e1071)
library(nnet)
library(stringr)
library(rlang)
library(Hmisc)
library(stringr)
library(lubridate)
library(knitr)
library(gapminder)
library(ggcorrplot)
library(waffle)
library(ICSNP)
library(factoextra)
library(MASS)
library(caTools)
library(e1071)
library(party)
library(simputation)
library(ggalt)
library(GGally)
library(ggmosaic)
library(psych)
library(rstatix )
library(nortest)
library(Boruta)
library(glmnet)
library(ggstatsplot)
library(bestNormalize)
library(car)
library(robustbase)
library(NeuralNetTools)
library(nnet)
library(performanceEstimation)
library(xgboost)
library(pROC)
library(rpart.plot)
library(randomForest)


data <- read.csv2("DAT.A.csv")
set.seed(123)
data <- delete_MCAR(data, 0.25, "Total_Amt_Chng_Q4_Q1")
data <- delete_MCAR(data, 0.20, "Education_Level")
data <- delete_MCAR(data, 0.10, "Total_Relationship_Count")
data <- delete_MCAR(data, 0.20, "Credit_Limit")
data <- delete_MCAR(data, 0.15, "Marital_Status")
data <- delete_MCAR(data, 0.15, "Gender")


data <- data[, -c((ncol(data) - 1):ncol(data))]
colnames(data)
colnames(data)<-str_to_sentence(colnames(data))
colnames(data) <- str_to_lower(colnames(data))
colnames(data)[c(1,12,13,15,17,18,19,20)] <- c("client_number","inactive_month_12_mon",
                                               "contacts_counts_12_mon","total_revolving_balance",
                                               "total_amount_changed","total_transaction_amount",
                                               "total_transaction_count","total_count_changed")

summary(data)
str(data)



categoric_colums_index <- c(2,4,6,7,8,9)
for (i in categoric_colums_index) {
  col_name <- colnames(data)[i]
  cat("Column:", col_name, "\n")
  for (var in unique(data[, i])) {
    count <- sum(data[, i] == var, na.rm = TRUE)
    cat("Variable:", var, "- Count:", count, "\n")
  }
  cat("\n")
}                                                  


data <- data %>% mutate(income_category = str_to_lower(income_category),
                        card_category =str_to_lower(card_category),
                        education_level = str_to_lower(education_level),
                        marital_status = str_to_lower(marital_status),
                        attrition_flag = str_to_lower(attrition_flag),
                        gender = str_to_lower(gender))

data <- data %>% mutate(gender=str_trim(gender,side="left"),
                        education_level=str_trim(education_level,side="left"),
                        marital_status=str_trim(marital_status,side="left"),
                        income_category=str_trim(income_category,side="left"),
                        card_category=str_trim(card_category,side="left"),
                        attrition_flag=str_trim(attrition_flag,side="left"))



data[data == "unknown"] <- NA

for (i in categoric_colums_index) {
  col_name <- colnames(data)[i]
  cat("Column:", col_name, "\n")
  for (var in unique(data[, i])) {
    count <- sum(data[, i] == var, na.rm = TRUE)
    cat("Variable:", var, "- Count:", count, "\n")
  }
  cat("\n")
} 


summary(data)
str(data)
describe(data)


data<-data %>% mutate(gender = as.factor(gender),
                      attrition_flag = as.factor(attrition_flag),
                      dependent_count = as.factor(dependent_count),
                      marital_status = as.factor(marital_status),
                      income_category = as.factor(income_category),
                      card_category = as.factor(card_category),
                      contacts_counts_12_mon = as.factor(contacts_counts_12_mon),
                      inactive_month_12_mon = as.factor(inactive_month_12_mon),
                      total_relationship_count = as.factor(total_relationship_count),
                      education_level = as.factor(education_level)
                      
)
data <- data %>% select(-client_number)