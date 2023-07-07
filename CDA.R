ki = table(data_fe$attrition_flag,data_fe$gender)
chisq.test(ki)
ki2 = table(data_fe$attrition_flag,data_fe$education_level)
chisq.test(ki2)
ki3 = table(data_fe$attrition_flag,data_fe$income_category)
chisq.test(ki3)
ki4 = table(data_fe$attrition_flag,data_fe$card_category)
chisq.test(ki4)
ki5 = table(data_fe$gender,data_fe$income_category)
chisq.test(ki5)
ki6 = table(data_fe$gender,data_fe$education_level)
chisq.test(ki6)
ki7 = table(data_fe$`inactive_month_12_mon`,data_fe$attrition_flag)
chisq.test(ki7)
ki8 = table(data_fe$`contacts_counts_12_mon`,data_fe$attrition_flag)
chisq.test(ki8)
ki9 = table(data_fe$total_relationship_count,data_fe$attrition_flag)
chisq.test(ki9)

ad.test(data_fe$credit_limit)
wilcox.test(credit_limit ~ attrition_flag,data = data_fe,exact=F)

ad.test(data_fe$customer_age)
wilcox.test(customer_age ~ attrition_flag,data= data_fe,exact=F)

ad.test(data_fe$months_on_book)
wilcox.test(months_on_book ~ attrition_flag,data= data_fe,exact=F)

ad.test(data_fe$total_revolving_balance)
wilcox.test(total_revolving_balance ~ attrition_flag,data = data_fe,exact=F)

ad.test(data_fe$avg_open_to_buy)
wilcox.test(avg_open_to_buy ~ attrition_flag,data = data_fe,exact=F)

ad.test(data_fe$total_amount_changed)
wilcox.test(total_amount_changed ~ attrition_flag,data = data_fe,exact=F)

ad.test(data_fe$total_transaction_amount)
wilcox.test(total_transaction_amount ~ attrition_flag,data = data_fe,exact=F)

ad.test(data_fe$total_transaction_count)
wilcox.test(total_transaction_count ~ attrition_flag,data = data_fe,exact=F)

ad.test(data_fe$total_count_changed)
wilcox.test(total_count_changed ~ attrition_flag,data = data_fe,exact=F)

ad.test(data_fe$avg_utilization_ratio)
wilcox.test(avg_utilization_ratio ~ attrition_flag,data = data_fe,exact=F)