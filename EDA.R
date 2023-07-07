t = table(data_fe$attrition_flag)
df = data.frame(t)
df

ggplot(df,aes(x=Var1,y=Freq)) +geom_segment(aes(x=Var1, xend=Var1, y=0, yend=Freq),size=1,color="blue", linetype="dotdash")+
  geom_point( color="skyblue", size=7, alpha=0.7, shape=21, stroke=2) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("freg")
help(linetype)

mosaicplot(table(data_fe$education_level,data_fe$attrition_flag),color = TRUE)
mosaicplot( ~ income_category+attrition_flag,data=data_fe,color = TRUE)
mosaicplot(table(data_fe$card_category,data_fe$attrition_flag),color = TRUE)
mosaicplot( ~ marital_status+attrition_flag,data=data_fe,color = TRUE)
mosaicplot( ~ gender+attrition_flag,data=data_fe,color = TRUE)
mosaicplot( ~ total_relationship_count+attrition_flag,data=data_fe,color = TRUE)
mosaicplot( ~ `inactive_month_12_mon`+attrition_flag,data=data_fe,color = TRUE)
mosaicplot( ~ `contacts_counts_12_mon`+attrition_flag,data=data_fe,color = TRUE)



plt <- ggbetweenstats(
  data = data_fe,
  x = attrition_flag,
  y = credit_limit
)
plt
descriptive_stats <- data_fe %>% filter(attrition_flag =="existing customer") 
a<-describe(descriptive_stats$credit_limit)
descriptive_stats2 <- data_fe %>% filter(attrition_flag =="attrited customer") 
b<-describe(descriptive_stats2$credit_limit)  
s <- rbind(a,b)
row.names(s) <- c("group 1","group 2")
print(s)


plt2 <- ggbetweenstats(
  data= data_fe,
  x = attrition_flag,
  y = customer_age
)
plt2
descriptive_stats <- data_fe %>% filter(attrition_flag =="existing customer") 
a<-describe(descriptive_stats$customer_age)
descriptive_stats2 <- data_fe %>% filter(attrition_flag =="attrited customer") 
b<-describe(descriptive_stats2$customer_age)  
s <- rbind(a,b)
row.names(s) <- c("group 1","group 2")
print(s)
pl3 <- ggbetweenstats(
  data = data_fe,
  x = attrition_flag,
  y = months_on_book
)
pl3
descriptive_stats <- data_fe %>% filter(attrition_flag =="existing customer") 
a<-describe(descriptive_stats$months_on_book)
descriptive_stats2 <- data_fe %>% filter(attrition_flag =="attrited customer") 
b<-describe(descriptive_stats2$months_on_book)  
s <- rbind(a,b)
row.names(s) <- c("group 1","group 2")
print(s)
plt4 <- ggbetweenstats(
  data = data_fe,
  x = attrition_flag,
  y = total_revolving_balance
)
plt4
descriptive_stats <- data_fe %>% filter(attrition_flag =="existing customer") 
a<-describe(descriptive_stats$total_revolving_balance)
descriptive_stats2 <- data_fe %>% filter(attrition_flag =="attrited customer") 
b<-describe(descriptive_stats2$total_revolving_balance)  
s <- rbind(a,b)
row.names(s) <- c("group 1","group 2")
print(s)
plt5 <- ggbetweenstats(
  data = data_fe,
  x = attrition_flag,
  y = avg_open_to_buy
)
plt5
descriptive_stats <- data_fe %>% filter(attrition_flag =="existing customer") 
a<-describe(descriptive_stats$avg_open_to_buy)
descriptive_stats2 <- data_fe %>% filter(attrition_flag =="attrited customer") 
b<-describe(descriptive_stats2$avg_open_to_buy)  
s <- rbind(a,b)
row.names(s) <- c("group 1","group 2")
print(s)
plt6 <- ggbetweenstats(
  data = data_fe,
  x = attrition_flag,
  y = total_amount_changed
)
plt6
descriptive_stats <- data_fe %>% filter(attrition_flag =="existing customer") 
a<-describe(descriptive_stats$total_amount_changed)
descriptive_stats2 <- data_fe %>% filter(attrition_flag =="attrited customer") 
b<-describe(descriptive_stats2$total_amount_changed)  
s <- rbind(a,b)
row.names(s) <- c("group 1","group 2")
print(s)
plt7 <- ggbetweenstats(
  data = data_fe,
  x = attrition_flag,
  y = total_transaction_amount
)
plt7
descriptive_stats <- data_fe %>% filter(attrition_flag =="existing customer") 
a<-describe(descriptive_stats$total_transaction_amount)
descriptive_stats2 <- data_fe %>% filter(attrition_flag =="attrited customer") 
b<-describe(descriptive_stats2$total_transaction_amount)  
s <- rbind(a,b)
row.names(s) <- c("group 1","group 2")
print(s)
plt8 <- ggbetweenstats(
  data = data_fe,
  x = attrition_flag,
  y = total_transaction_count
)
plt8
descriptive_stats <- data_fe %>% filter(attrition_flag =="existing customer") 
a<-describe(descriptive_stats$total_transaction_count)
descriptive_stats2 <- data_fe %>% filter(attrition_flag =="attrited customer") 
b<-describe(descriptive_stats2$total_transaction_count)  
s <- rbind(a,b)

row.names(s) <- c("group 1","group 2")
print(s)
plt9 <- ggbetweenstats(
  data = data_fe,
  x = attrition_flag,
  y = total_count_changed
)
plt9
descriptive_stats <- data_fe %>% filter(attrition_flag =="existing customer") 
a<-describe(descriptive_stats$total_count_changed)
descriptive_stats2 <- data_fe %>% filter(attrition_flag =="attrited customer") 
b<-describe(descriptive_stats2$total_count_changed)  
s <- rbind(a,b)
row.names(s) <- c("group 1","group 2")
print(s)
plt10 <- ggbetweenstats(
  data = data_fe,
  x = attrition_flag,
  y = avg_utilization_ratio
)
plt10
descriptive_stats <- data_fe %>% filter(attrition_flag =="existing customer") 
a<-describe(descriptive_stats$avg_utilization_ratio)
descriptive_stats2 <- data_fe %>% filter(attrition_flag =="attrited customer") 
b<-describe(descriptive_stats2$avg_utilization_ratio)  
s <- rbind(a,b)
row.names(s) <- c("group 1","group 2")
print(s)

x<- data_fe %>% select_if(is.numeric)
x <- na.omit(x)
res <- cor(x, method="pearson")
corrplot::corrplot(res, method= "color", order = "hclust")

x1 <-  subset(x, select = c( "customer_age","avg_utilization_ratio","avg_open_to_buy",
                             "months_on_book"))
GGally::ggpairs(x1)

dfmel <- data_fe %>% select(c(total_transaction_count,total_transaction_amount))

# Groups
species <- data_fe$attrition_flag

# Number of groups
l <- length(unique(species))

plot(dfmel,
     pch = 20,
     bg = hcl.colors(l, "Temps")[species],
     col = hcl.colors(l, "Temps")[species])

