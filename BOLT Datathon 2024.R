library(dplyr)
library(tidyverse)
library(lubridate)

result_table1 <- BOLT_Data_Set %>% select("Fraud Indicator (Yes/No)", "Card Present Status")
colnames(result_table1) <- c("fraud_ind", "card_present")

result_table1 <- result_table1 %>%
  group_by(fraud_ind, card_present) %>%
  summarise(count = n())
print(result_table1)

result_table2 <- BOLT_Data_Set %>% select("Fraud Indicator (Yes/No)", "Chip Usage")
colnames(result_table2) <- c("fraud_ind", "chip_usage")
result_table2 <- result_table2 %>%
  group_by(fraud_ind, chip_usage) %>%
  summarise(count = n())
print(result_table2)

result_table3 <- BOLT_Data_Set %>% select("Fraud Indicator (Yes/No)", "Cross-border Transaction (Yes/No)")
colnames(result_table3) <- c("fraud_ind", "cross_border")
result_table3 <- result_table3 %>%
  group_by(fraud_ind, cross_border) %>%
  summarise(count = n())
print(result_table3)

numerical <- BOLT_Data_Set %>% select("Risk Assessment", "Transaction Value", "Fraud Indicator (Yes/No)")
colnames(numerical) <- c("risk", "money_val", "fraud")

risk_assessment_yes <- numerical %>%
  filter(fraud == "Yes") %>%
  ggplot() +
  geom_histogram(aes(x = risk))
print(risk_assessment_yes)

risk_assessment_no <- numerical %>%
  filter(fraud == "No") %>%
  ggplot() +
  geom_histogram(aes(x = risk))
print(risk_assessment_no)

value_yes <- numerical %>%
  filter(fraud == "Yes") %>%
  ggplot() +
  geom_histogram(aes(x = money_val))
print(value_yes)

value_no <- numerical %>%
  filter(fraud == "No") %>%
  ggplot() +
  geom_histogram(aes(x = money_val))
print(value_no)

result_table4 <- BOLT_Data_Set %>% select("Fraud Indicator (Yes/No)", "Payment Method")
colnames(result_table4) <- c("fraud_ind", "pay_method")
result_table4 <- result_table4 %>%
  group_by(fraud_ind, pay_method) %>%
  summarise(count = n())
print(result_table4)

# leaves 97963 rows
result_table5 <- BOLT_Data_Set %>% select("Fraud Indicator (Yes/No)", "Transaction Date", "Transaction Time")
colnames(result_table5) <- c("fraud_ind", "date", "time_of_day")
result_table5$time_of_day <- substr(result_table5$time_of_day, start = 1, stop = 2)
result_table5 <- result_table5 %>% filter(time_of_day != "na")
print(result_table5)
# NEXT STEPS: filter out the <23 values, then plot histogram based on time of day