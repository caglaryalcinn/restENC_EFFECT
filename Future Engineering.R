data_fe <- data %>% mutate(months_on_book = ifelse(months_on_book == 36, NA, months_on_book))

mel <- data.frame(data_fe$months_on_book,data_fe$customer_age)
imp1 <-mice(data = mel ,m=5,maxit=5,meth = "pmm",seed=500)

mel <- complete(imp1,1)
data_fe$months_on_book <- mel$data_fe.months_on_book

data_fe <- data_fe %>% mutate(avg_utilization_ratio = ifelse(avg_utilization_ratio == 0, NA, avg_utilization_ratio))


data_fe <- data_fe %>% mutate(total_revolving_balance = ifelse(total_revolving_balance == 0, NA, total_revolving_balance))
data_fe <- data_fe %>% mutate(total_revolving_balance = ifelse(total_revolving_balance == 2517, NA, total_revolving_balance))

data_fe <- data_fe %>% mutate(credit_limit = ifelse(credit_limit == 34516, NA, credit_limit))
data_fe <- data_fe %>% mutate(credit_limit = ifelse(credit_limit == 1438.3, NA, credit_limit))
data_fe <- data_fe %>% mutate(credit_limit = ifelse(avg_open_to_buy == 34516, NA, credit_limit))




trans1 <- bestNormalize(data_fe$credit_limit)
trans2 <- bestNormalize(data_fe$avg_utilization_ratio)
trans3 <- bestNormalize(data_fe$avg_open_to_buy)
trans4 <- bestNormalize(data_fe$total_count_changed)
trans5 <- bestNormalize(data_fe$total_amount_changed)
trans6 <- bestNormalize(data_fe$total_transaction_amount)
trans7 <- bestNormalize(data_fe$total_revolving_balance)

### we get better disstibitions using order log but we remain karekök dağılımı ile devam ediyoruz 
data_fe$credit_limit<-trans1$other_transforms$sqrt_x$x.t
data_fe$avg_utilization_ratio<-trans2$other_transforms$sqrt_x$x.t
data_fe$avg_open_to_buy<-trans3$other_transforms$sqrt_x$x.t
data_fe$total_count_changed<-trans4$other_transforms$sqrt_x$x.t
data_fe$total_amount_changed<-trans5$other_transforms$sqrt_x$x.t
data_fe$total_transaction_amount<-trans6$other_transforms$sqrt_x$x.t
data_fe$total_revolving_balance<-trans7$other_transforms$sqrt_x$x.t


scaler <- function(x){
  (x - median(x)) / (mad(x))
}
data_fe <- data_fe %>%
  mutate_at(c("customer_age","months_on_book","total_transaction_count"), scaler)
