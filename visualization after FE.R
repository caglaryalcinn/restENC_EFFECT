ggp3_ <- ggplot(data_fe, aes(x = credit_limit)) +    # Draw histogram & density
  geom_histogram(fill="#69b3a2",aes(y = ..density..)) + 
  geom_density(col = "#1b98e0", size = 2) 

ggp3_ 


ggp3_ <- ggplot(data_fe, aes(x = avg_open_to_buy)) +    # Draw histogram & density
  geom_histogram(fill="#69b3a2",aes(y = ..density..)) + 
  geom_density(col = "#1b98e0", size = 2) 

ggp3_ 


ggp3_ <- ggplot(data_fe, aes(x = total_transaction_amount)) +    # Draw histogram & density
  geom_histogram(fill="#69b3a2",aes(y = ..density..)) + 
  geom_density(col = "#1b98e0", size = 2) 

ggp3_ 

ggp3 <- ggplot(data_fe, aes(x = total_transaction_count)) +    # Draw histogram & density
  geom_histogram(fill="#69b3a2",aes(y = ..density..)) + 
  geom_density(col = "#1b98e0", size = 2) 

ggp3_ 


ggp3_ <- ggplot(data_fe, aes(x = total_count_changed)) +    # Draw histogram & density
  geom_histogram(fill="#69b3a2",aes(y = ..density..)) + 
  geom_density(col = "#1b98e0", size = 2) 

ggp3_ 

ggp3 <- ggplot(data_fe, aes(x = total_amount_changed)) +    # Draw histogram & density
  geom_histogram(fill="#69b3a2",aes(y = ..density..)) + 
  geom_density(col = "#1b98e0", size = 2) 

ggp3_ 

ggp3_ <- ggplot(data_fe, aes(x = avg_utilization_ratio)) +    # Draw histogram & density
  geom_histogram(fill="#69b3a2",aes(y = ..density..)) + 
  geom_density(col = "#1b98e0", size = 2) 

ggp3_ 

ggp3_ <- ggplot(data_fe, aes(x = total_revolving_balance)) +    # Draw histogram & density
  geom_histogram(fill="#69b3a2",aes(y = ..density..)) + 
  geom_density(col = "#1b98e0", size = 2) 

ggp3_ 