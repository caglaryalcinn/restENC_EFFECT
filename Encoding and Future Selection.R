extracted_column <- as.data.frame(data_fe$attrition_flag)
remaining_data_fe <- subset(data_fe, select = -attrition_flag)
#extracted_column <- ifelse(extracted_column == "existing customer", "1", 
#                           ifelse(extracted_column == "attrited customer", "0", extracted_column))
#extracted_column <- as.numeric(extracted_column)
extracted_column <- data.frame(extracted_column)

dummy <- dummyVars(" ~ .", data=remaining_data_fe)
data_fe_ohe1 <- data.frame(predict(dummy, newdata=remaining_data_fe))
data_fe_ohe <- cbind(data_fe_ohe1,extracted_column)
colnames(data_fe_ohe)[57] <- "attrition_flag"
data_fe_ohe <- data_fe_ohe %>%
  select(attrition_flag, everything())
x <- model.matrix(attrition_flag~., data_fe_ohe)[,-1]
y <- ifelse(data_fe_ohe$attrition_flag == "existing customer", 1, 0)
library(car)

set.seed(24)
cv.lasso <- cv.glmnet(x, data_fe_ohe$attrition_flag, alpha = 1, family = "binomial")
plot(cv.lasso)
cv.lasso$lambda.min

best_model <- glmnet(x, y, alpha = 1, lambda = cv.lasso$lambda.min)
coef(best_model)
print(best_model)
y_predicted <- predict(best_model, s = cv.lasso$lambda.min, newx = x)

sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

rsq <- 1 - sse/sst
rsq

set.seed(23)
boruta.dia2_train <- Boruta(attrition_flag~., data = data_fe_ohe, doTrace = 1)
print(boruta.dia2_train)

boruta.dia2 <- TentativeRoughFix(boruta.dia2_train)
print(boruta.dia2)

plot(boruta.dia2, xlab = "", xaxt = "n")


getSelectedAttributes(boruta.dia2, withTentative = F)
data.fs = data_fe_ohe[, c("gender.f", "marital_status.single", "card_category.blue", "inactive_month_12_mon.0", "inactive_month_12_mon.3", "contacts_counts_12_mon.4", "credit_limit", "total_amount_changed", "total_count_changed", "customer_age", "dependent_count.0", "income_category..60k....80k", "card_category.silver", "total_relationship_count.2", "total_relationship_count.5", "inactive_month_12_mon.1", "inactive_month_12_mon.4", "contacts_counts_12_mon.2", "contacts_counts_12_mon.5", "total_revolving_balance", "marital_status.married","income_category.less.than..40k","months_on_book",
                          "total_relationship_count.3","total_relationship_count.6","inactive_month_12_mon.2","contacts_counts_12_mon.0","contacts_counts_12_mon.3","contacts_counts_12_mon.6","total_transaction_count")]
data.fs <- cbind(data.fs,extracted_column)
colnames(data.fs)[31] <- "attrition_flag"
data.fs <- data.fs %>%
  select(attrition_flag, everything())