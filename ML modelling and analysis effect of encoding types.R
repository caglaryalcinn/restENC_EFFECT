##svm
set.seed(412)

####WİTHOUT ENCODİNG
train_data2$attrition_flag <-as.factor(train_data2$attrition_flag)
levels(test_data2$attrition_flag) <- make.names(levels(train_data2$attrition_flag))

tunegrid <- expand.grid(C = c(1^( 2:9)),sigma = c(0.001 ,.005 ,0.01 ,0.1))

modelsvm <- train(attrition_flag ~ ., data =train_data2, method = "svmRadial",tuneGrid=tunegrid ,trControl = ctrl,search = "grid")
modelsvm$bestTune
pred.svm <- predict(modelsvm, newdata = train_data2,type ="raw")
cm<- confusionMatrix(train_data2$attrition_flag,pred.svm)

cm

pred.svm <- predict(modelsvm, newdata = test_data2,type ="raw")
cm.<- confusionMatrix(test_data2$attrition_flag,pred.svm)

cm.

roc_score4=roc(test_data4$attrition_flag, as.numeric(pred.svm))
plot(roc_score4)
plot(modelsvm)
#for plott
model = svm(attrition_flag ~ ., data = train_data4,kernel = 'radial', cost =1,sigma =0.1)
plot(model,train_data4,total_transaction_count~total_transaction_amount,color.palette = heat.colors)



####WİT ENCODİNG
train_data4$attrition_flag <-as.factor(train_data4$attrition_flag)
levels(test_data2$attrition_flag) <- make.names(levels(train_data2$attrition_flag))

modelsvm2 <- train(attrition_flag ~ ., data =train_data4, method = "svmRadial",tuneGrid=tunegrid, trControl = ctrl,search = "grid")
modelsvm2$bestTune
pred.svm2 <- predict(modelsvm2, newdata = train_data4,type ="raw")
cm2<- confusionMatrix(train_data2$attrition_flag,pred.svm2)

cm2

pred.svm2 <- predict(modelsvm2, newdata = test_data4,type ="raw")
cm.2<- confusionMatrix(test_data4$attrition_flag,pred.svm2)

cm.2

roc_score4.2=roc(test_data4$attrition_flag, as.numeric(pred.svm2))
plot(roc_score4)
plot(modelsvm2)
#for plott
model2 = svm(attrition_flag ~ ., data = train_data4,kernel = 'radial', cost =1,sigma =0.1)
plot(model2,train_data4,total_transaction_count~total_transaction_amount,color.palette = heat.colors)

##weigted svm
svm_model_weighted <- svm(attrition_flag ~ ., data = train_data4,type = "C-classification", kernel = 'linear', cost =1,class.weights = c("attrited.customer" = 5, "existing.customer" = 1))
pred_svm_wg = predict(svm_model_weighted, newdata = test_data4)
attributes(pred_svm_wg )$names <- NULL
# pred_svm_wg  <- factor(pred_svm_wg , levels = levels(test_data4$attrition_flag))
confusion_matrix_svm2 <- confusionMatrix(data = pred_svm_wg, reference = test_data4$attrition_flag)
confusion_matrix_svm2
roc_score3=roc(test_data4$attrition_flag, as.numeric(pred_svm_wg)) ##0.8598
plot(roc_score3)
plot(svm_model_weighted,train_data4,total_transaction_count~total_transaction_amount,color.palette = heat.colors)

## RF

### without encoding



set.seed(132)
tunegrid <- expand.grid(.mtry=c(1:25))
model3 <- train(attrition_flag~., data = train_data2,metrİC = "Kappa" ,method = "rf", tuneGrid=tunegrid,
                trControl = ctrl)

set.seed(120)  # Setting seed
classifier_RF = randomForest(x = train_data2[-1],
                             y = train_data2$attrition_flag,
                             mtyr = 16,
                             ntree = 500)
classifier_RF
set.seed(145)
pred_rf2 = predict(classifier_RF, newdata = train_data2)
pred_rf = predict(classifier_RF, newdata = test_data2)
confusion_matrix_rf2 <- confusionMatrix(data = pred_rf2, reference = train_data2$attrition_flag)
confusion_matrix_rf2
confusion_matrix_rf <- confusionMatrix(data = pred_rf, reference = test_data2$attrition_flag)
confusion_matrix_rf
roc_score2=roc(test_data2$attrition_flag, as.numeric(pred_rf)) ##0.8941
plot(roc_score2)
plot(model3)
plot(classifier_RF)
varimp(classifier_RF)

##with encoding

model3 <- train(attrition_flag~., data = train_data4,metrİC = "Kappa" ,method = "rf", tuneGrid=tunegrid,
                trControl = ctrl)

set.seed(120)  # Setting seed
classifier_RF = randomForest(x = train_data4[-1],
                             y = train_data4$attrition_flag,
                             mtyr = 16,
                             ntree = 500)
classifier_RF
set.seed(145)
pred_rf = predict(classifier_RF, newdata = train_data4)

confusion_matrix_rf <- confusionMatrix(data = pred_rf, reference = train_data4$attrition_flag)
confusion_matrix_rf

pred_rf2 = predict(classifier_RF, newdata = test_data4)

confusion_matrix_rf <- confusionMatrix(data = pred_rf2, reference = test_data4$attrition_flag)
confusion_matrix_rf

roc_score2=roc(test_data4$attrition_flag, as.numeric(pred_rf2)) ##0.8941
plot(roc_score2)
plot(varimp(classifier_RF))

varImpPlot(classifier_RF)

### XGBoost

#without Encoding
set.seed(123)
tunegrid <-  expand.grid(max_depth = c(3, 5, 7), 
                         nrounds = (1:10)*50,    # number of trees
                         # default values below
                         eta = 0.3,
                         gamma = 0,
                         subsample = 1,
                         min_child_weight = 1,
                         colsample_bytree = 0.6)
modelxg <- train(
  attrition_flag ~., data = train_data2, method = "xgbTree",metric ="Kappa",tuneGrid=tunegrid,
  trControl = ctrl
)

# Best tuning parameter
modelxg$bestTune

predicted.classes3 <- modelxg %>% predict(train_data2)
head(predicted.classes3)

confusion_matrix_xg3 <- confusionMatrix(data = predicted.classes3, reference = train_data2$attrition_flag)

predicted.classes <- modelxg %>% predict(test_data2)
head(predicted.classes)

confusion_matrix_xg <- confusionMatrix(data = predicted.classes, reference = test_data2$attrition_flag)

roc_score=roc(test_data2$attrition_flag, as.numeric(predicted.classes)) ##0.9184
plot(roc_score)
help(varImp)
varImp(modelxg)
plot(varImp(modelxg))

install.packages("DiagrammeR")
xgb.plot.tree(model = modelxg$finalModel,trees = 10)

#### with encoding

set.seed(123)
modelxg2 <- train(
  attrition_flag ~., data = train_data4, method = "xgbTree",metric ="Kappa",tuneGrid=tunegrid,
  trControl = ctrl
)
# Best tuning parameter
modelxg2$bestTune

predicted.classes2 <- modelxg2 %>% predict(train_data4)
head(predicted.classes2)

confusion_matrix_xg2 <- confusionMatrix(data = predicted.classes2, reference = train_data4$attrition_flag)

predicted.classes2.<- modelxg2 %>% predict(test_data4)
head(predicted.classes2.)

confusion_matrix_xg2. <- confusionMatrix(data = predicted.classes2., reference = test_data4$attrition_flag)

roc_score=roc(test_data4$attrition_flag, as.numeric(predicted.classes2.)) ##0.9184
plot(roc_score)
help(varImp)
varImp(modelxg)
plot(varImp(modelxg2))


xgb.plot.tree(model = modelxg$finalModel,trees = 10)

###ANN

## WİTHOUT ENCODİNG
set.seed(26)
model_nn <-train(attrition_flag ~.,  data =train_data2, method = "nnet", trControl = ctrl,preProcess = c("center"))
model_nn$bestTune #5,,,0,1
pred_nn = predict(model_nn, newdata = train_data2,type = "raw")

confusion_matrix_nn <- confusionMatrix(data = pred_nn, reference = train_data2$attrition_flag)
confusion_matrix_nn

pred_nn2 = predict(model_nn, newdata = test_data2,type = "raw")

confusion_matrix_nn2 <- confusionMatrix(data = pred_nn2, reference = test_data2$attrition_flag)
confusion_matrix_nn2

roc_score2=roc(test_data2$attrition_flag, as.numeric(pred_nn2)) ## 0.8494
plot(roc_score2)
plot(model_nn)

model_nn$finalModel
plotnet(model_nn)

#WİTH ENCODİNG

model_nn2 <-train(attrition_flag ~.,  data =train_data4, method = "nnet", trControl = ctrl,preProcess = c("center"))
model_nn2$bestTune #5,,,0,1
pred_nn. = predict(model_nn2, newdata = train_data4,type = "raw")

confusion_matrix_nn. <- confusionMatrix(data = pred_nn., reference = train_data4$attrition_flag)
confusion_matrix_nn.
pred_nn.2 = predict(model_nn2, newdata = test_data4,type = "raw")

confusion_matrix_nn.2 <- confusionMatrix(data = pred_nn.2, reference = test_data4$attrition_flag)
confusion_matrix_nn.2
roc_score2=roc(test_data4$attrition_flag, as.numeric(pred_nn.2)) ## 0.8494
plot(roc_score2)
plot(model_nn)

model_nn$finalModel
plotnet(model_nn)