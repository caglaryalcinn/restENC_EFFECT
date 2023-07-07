ggp3 <- ggplot(data, aes(x = customer_age)) +    # Draw histogram & density
  geom_histogram(fill="#69b3a2",aes(y = ..density..),binwidth=6) + 
  geom_density(col = "#1b98e0", size = 2) 

ggp3


ggp3 <- ggplot(data, aes(x = credit_limit)) +    # Draw histogram & density
  geom_histogram(fill="#69b3a2",aes(y = ..density..)) + 
  geom_density(col = "#1b98e0", size = 2) 

ggp3


ggp3 <- ggplot(data, aes(x = avg_open_to_buy)) +    # Draw histogram & density
  geom_histogram(fill="#69b3a2",aes(y = ..density..)) + 
  geom_density(col = "#1b98e0", size = 2) 

ggp3


ggp3 <- ggplot(data, aes(x = months_on_book)) +    # Draw histogram & density
  geom_histogram(fill="#69b3a2",aes(y = ..density..),binwidth=6) + 
  geom_density(col = "#1b98e0", size = 2) 

ggp3



ggp3 <- ggplot(data, aes(x = total_transaction_amount)) +    # Draw histogram & density
  geom_histogram(fill="#69b3a2",aes(y = ..density..)) + 
  geom_density(col = "#1b98e0", size = 2) 

ggp3

ggp3 <- ggplot(data, aes(x = total_transaction_count)) +    # Draw histogram & density
  geom_histogram(fill="#69b3a2",aes(y = ..density..)) + 
  geom_density(col = "#1b98e0", size = 2) 

ggp3


ggp3 <- ggplot(data, aes(x = total_count_changed)) +    # Draw histogram & density
  geom_histogram(fill="#69b3a2",aes(y = ..density..)) + 
  geom_density(col = "#1b98e0", size = 2) 

ggp3

ggp3 <- ggplot(data, aes(x = total_amount_changed)) +    # Draw histogram & density
  geom_histogram(fill="#69b3a2",aes(y = ..density..)) + 
  geom_density(col = "#1b98e0", size = 2) 

ggp3

ggp3 <- ggplot(data, aes(x = avg_utilization_ratio)) +    # Draw histogram & density
  geom_histogram(fill="#69b3a2",aes(y = ..density..)) + 
  geom_density(col = "#1b98e0", size = 2) 

ggp3

ggp3 <- ggplot(data, aes(x = total_revolving_balance)) +    # Draw histogram & density
  geom_histogram(fill="#69b3a2",aes(y = ..density..)) + 
  geom_density(col = "#1b98e0", size = 2) 

ggp3


ggplot(data, aes(x=customer_age)) + 
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

ggplot(data, aes(x=credit_limit)) + 
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

ggplot(data, aes(x=avg_open_to_buy)) + 
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

ggplot(data, aes(x=months_on_book)) + 
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

ggplot(data, aes(x=total_transaction_amount)) + 
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

ggplot(data, aes(x=total_transaction_count)) + 
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

ggplot(data, aes(x=total_count_changed)) + 
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

ggplot(data, aes(x=total_amount_changed)) + 
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

ggplot(data, aes(x=avg_utilization_ratio)) + 
  geom_boxplot(
    
    color="blue",
    fill="blue",
    alpha=0.2,
    
    notch=TRUE,
    notchwidth = 0.8,
    
    outlier.colour="red",
    outlier.fill="red",
    outlier.size=3)

ggplot(data, aes(x=total_revolving_balance)) + 
  geom_boxplot(
    
    color="blue",
    fill="blue",
    alpha=0.2,
    
    notch=TRUE,
    notchwidth = 0.8,
    
    outlier.colour="red",
    outlier.fill="red",
    outlier.size=3)