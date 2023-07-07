str(data_fe)
vis_miss(data_fe)
gg_miss_var(data_fe)
gg_miss_var(data_fe, facet = attrition_flag)
gg_miss_upset(data_fe)
library(mice)
impo <- mice(data= data ,m=1,maxit=1,meth = "pmm",seed=500)
data <- complete(impo,1)
imputing <- mice(data= data_fe ,m=1,maxit=1,meth = "pmm",seed=500)
data_fe <- complete(imputing,1)


densityplot(imputing)
data_fe <- complete(imputing,1)
vis_miss(data_fe)
#install.packages("devtools")
#devtools::install_github("amices/ggmice")
library(ggmice)
ggmice(imputing, aes(credit_limit, total_transaction_amount)) +
  geom_point()
ggmice(imputing, aes(education_level)) +
  geom_bar()
ggmice(imputing, aes(marital_status)) +
  geom_bar()

ggmice(imputing, aes(x = .imp, y = total_amount_changed)) +
  geom_jitter(height = 0) +
  geom_boxplot(fill = "white", alpha = 0.75, outlier.shape = NA) +
  labs(x = "Imputation number")