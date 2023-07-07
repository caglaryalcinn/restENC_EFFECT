train_indices2 <- createDataPartition(data_fe$attrition_flag, p = 0.8, list = FALSE)
train_data2 <- data_fe[train_indices2, ]
test_data2<- data_fe[-train_indices2, ]
train_indices4 <- createDataPartition(data_fe_ohe$attrition_flag, p = 0.8, list = FALSE)
train_data4 <- data_fe_ohe[train_indices2, ]
test_data4<- data_fe_ohe[-train_indices2, ]
ctrl <- trainControl(method="cv", 
                     number=10, 
                     savePredictions="all",
                     classProbs=TRUE)