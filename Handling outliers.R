data_fe <- data_fe %>% 
  mutate(
    total_count_changed = ifelse(
      total_count_changed > quantile(total_count_changed, 0.65, na.rm = TRUE) + 1.5 * IQR(total_count_changed, na.rm = TRUE),
      max(total_count_changed[total_count_changed != max(total_count_changed)]),total_count_changed 
    ),
    ifelse(
      total_count_changed < quantile(total_count_changed, 0.45, na.rm = TRUE) - 1.5 * IQR(total_count_changed, na.rm = TRUE),
      min(total_count_changed[total_count_changed != min(total_count_changed)]),
      total_count_changed  
    )
  )
data_fe$total_count_changed <- data_fe$`ifelse(...)`
data_fe <- data_fe %>% select(-`ifelse(...)`)

data_fe <- data_fe %>% 
  mutate(
    total_transaction_count = ifelse(
      total_transaction_count > quantile(total_transaction_count, 0.6, na.rm = TRUE) + 1.5 * IQR(total_transaction_count, na.rm = TRUE),
      max(total_transaction_count[total_transaction_count != max(total_transaction_count)]),
      total_transaction_count  
    ),
    ifelse(
      total_transaction_count < quantile(total_transaction_count, 0.4, na.rm = TRUE) - 1.5 * IQR(total_transaction_count, na.rm = TRUE),
      min(total_transaction_count[total_transaction_count != min(total_transaction_count)]),
      total_transaction_count  
    )
  )

data_fe$total_transaction_count <- data_fe$`ifelse(...)`
data_fe <- data_fe %>% select(-`ifelse(...)`)

data_fe <- data_fe %>% 
  mutate(
    total_amount_changed = ifelse(
      total_amount_changed > quantile(total_amount_changed, 0.65, na.rm = TRUE) + 1.5 * IQR(total_amount_changed, na.rm = TRUE),
      max(total_amount_changed[total_amount_changed != max(total_amount_changed)]),
      total_amount_changed  
    ),
    ifelse(
      total_amount_changed < quantile(total_amount_changed, 0.45 ,na.rm = TRUE) - 1.5 * IQR(total_amount_changed, na.rm = TRUE),
      min(total_amount_changed[total_amount_changed != min(total_amount_changed)]),
      total_amount_changed  
    )
  )

data_fe$total_amount_changed <- data_fe$`ifelse(...)`
data_fe <- data_fe %>% select(-`ifelse(...)`)


data_fe <- data_fe %>% 
  mutate(
    total_transaction_amount = ifelse(
      total_transaction_amount > quantile(total_transaction_amount, 0.6, na.rm = TRUE) + 1.5 * IQR(total_transaction_amount, na.rm = TRUE),
      max(total_transaction_amount[total_transaction_amount != max(total_transaction_amount)]),
      total_transaction_amount  # Koşul yanlış olduğunda döndürülecek değeri ekleyin
    ),
    ifelse(
      total_transaction_amount < quantile(total_transaction_amount, 0.4 ,na.rm = TRUE) - 1.5 * IQR(total_transaction_amount, na.rm = TRUE),
      min(total_transaction_amount[total_transaction_amount != min(total_transaction_amount)]),
      total_transaction_amount  # Koşul yanlış olduğunda döndürülecek değeri ekleyin
    )
  )
data_fe$total_transaction_amount <- data_fe$`ifelse(...)`
data_fe <- data_fe %>% select(-`ifelse(...)`)

data_fe <- data_fe %>% 
  mutate(
    total_revolving_balance = ifelse(
      total_revolving_balance > quantile(total_revolving_balance, 0.65, na.rm = TRUE) + 1.5 * IQR(total_revolving_balance, na.rm = TRUE),
      max(total_revolving_balance[total_revolving_balance != max(total_revolving_balance)]),
      total_revolving_balance  # Koşul yanlış olduğunda döndürülecek değeri ekleyin
    ),
    ifelse(
      total_revolving_balance < quantile(total_revolving_balance, 0.45 ,na.rm = TRUE) - 1.5 * IQR(total_revolving_balance, na.rm = TRUE),
      min(total_revolving_balance[total_revolving_balance != min(total_revolving_balance)]),
      total_revolving_balance  # Koşul yanlış olduğunda döndürülecek değeri ekleyin
    )
  )

data_fe$total_revolving_balance <- data_fe$`ifelse(...)`
data_fe <- data_fe %>% select(-`ifelse(...)`)
#### check 
ggplot(data_fe, aes(x=total_transaction_amount)) + 
  geom_boxplot(
    
    color="blue",
    fill="blue",
    alpha=0.2,
    
    notch=TRUE,
    notchwidth = 0.8,
    
    outlier.colour="red",
    outlier.fill="red",
    outlier.size=3
  )

ggplot(data_fe, aes(x=total_amount_changed)) + 
  geom_boxplot(
    
    color="blue",
    fill="blue",
    alpha=0.2,
    
    notch=TRUE,
    notchwidth = 0.8,
    
    outlier.colour="red",
    outlier.fill="red",
    outlier.size=3
  )

ggplot(data_fe, aes(x=total_transaction_count)) + 
  geom_boxplot(
    
    color="blue",
    fill="blue",
    alpha=0.2,
    
    notch=TRUE,
    notchwidth = 0.8,
    
    outlier.colour="red",
    outlier.fill="red",
    outlier.size=3)

ggplot(data_fe, aes(x=total_count_changed)) + 
  geom_boxplot(
    
    color="blue",
    fill="blue",
    alpha=0.2,
    
    notch=TRUE,
    notchwidth = 0.8,
    
    outlier.colour="red",
    outlier.fill="red",
    outlier.size=3)
ggplot(data_fe, aes(x=total_revolving_balance)) + 
  geom_boxplot(
    
    color="blue",
    fill="blue",
    alpha=0.2,
    
    notch=TRUE,
    notchwidth = 0.8,
    
    outlier.colour="red",
    outlier.fill="red",
    outlier.size=3)