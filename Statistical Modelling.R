
str(train_data2)
set.seed(1985)
train_data2$attrition_flag <-as.factor(train_data2$attrition_flag)
levels(test_data2$attrition_flag) <- make.names(levels(train_data2$attrition_flag))
set.seed(1985)


levels(train_data2$attrition_flag) <- make.names(levels(train_data2$attrition_flag))
train_data2$attrition_flag <- as.factor(train_data2$attrition_flag)
modelglm <- train(attrition_flag ~ ., data=train_data2, 
                  method="glm", 
                  family=binomial, 
                  trControl=ctrl)
summary(modelglm)
pred.glm <- predict(modelglm, newdata = train_data2,type ="raw")
levels(train_data2$attrition_flag) <- make.names(levels(train_data2$attrition_flag))

cm<- confusionMatrix(train_data2$attrition_flag,pred.glm)
cm

pred.glm <- predict(modelglm, newdata = test_data2,type ="raw")
levels(test_data2$attrition_flag) <- make.names(levels(test_data2$attrition_flag))

cm<- confusionMatrix(test_data2$attrition_flag,pred.glm)
cm



roc_score=roc(test_data2$attrition_flag, as.numeric(pred.glm))
plot(roc_score)
vif(modelglm$finalModel)
varImp(modelglm)
#ld.vars <- attributes(alias(modelglm)$Complete)$dimnames[[1]]


##for detect linearity
probabilities <- predict(modelglm$finalModel, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
head(predicted.classes)
mydata <- train_data2 %>%
  dplyr::select_if(is.numeric)
predictors <- colnames(mydata)
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
mydata$predictor.value <- sin(mydata$predictor.value)
ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") +
  theme_bw() +
  facet_wrap(~predictors, scales = "free_y")

hist((mydata$logit)) ## for detect distribition of response variable

plot(modelglm$finalModel, which = 4, id.n = 3) ### for detect outlier influence  points and removing
model.datafin <- augment(modelglm$finalModel) %>% 
  mutate(index = 1:n())
model.datafin %>% top_n(3, .cooksd)
ggplot(model.datafin, aes(index, .std.resid)) + 
  geom_point(aes(color = .outcome), alpha = .5) +
  theme_bw()
model.datafin %>% 
  filter(abs(.std.resid) > 1)
summary(modelglm$finalModel)
train_data_glm <- model.datafin %>% select(c(genderm,dependent_count5,dependent_count3,total_revolving_balance,total_transaction_amount,
                                             total_transaction_count,`education_levelpost-graduate`
))
colnames(train_data_glm)[1] <- "attrition_flag"
a<-cbind(model.datafin$.outcome,train_data_glm)
colnames(a)[1] <- "attrition_flag"
modelglm2 <- train(attrition_flag ~ ., data=a, 
                   method="glm", 
                   family=binomial, 
                   trControl=ctrl)
summary(modelglm2)
vif(modelglm2$finalModel)
probabilities <- predict(modelglm$finalModel, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
head(predicted.classes)
mydata <- train_data2 %>%
  dplyr::select_if(is.numeric)
predictors <- colnames(mydata)
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
mydata$predictor.value <- sin(mydata$predictor.value)
ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") +
  theme_bw() +
  facet_wrap(~predictors, scales = "free_y")

hist((mydata$logit)) ## for detect distribition of response variable

plot(modelglm$finalModel, which = 4, id.n = 3) ### for detect outlier influence  points and removing
model.datafin <- augment(modelglm$finalModel) %>% 
  mutate(index = 1:n())
model.datafin %>% top_n(3, .cooksd)
ggplot(model.datafin, aes(index, .std.resid)) + 
  geom_point(aes(color = .outcome), alpha = .5) +
  theme_bw()
model.datafin %>% 
  filter(abs(.std.resid) > 1)
summary(modelglm$finalModel)