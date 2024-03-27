library(dplyr)
library(tidyverse)
library(lubridate)

dataset <- read.csv("clean_BOLT_dataset.csv")

# fraud ~4x more likely with card NOT present than with card present
card_or_not <- dataset %>% select(fraud_flagged, card_present)
card_or_not <- card_or_not %>%
  group_by(fraud_flagged, card_present) %>%
  summarise(count = n())
print(card_or_not)

# fraud ~6x more likely without chip used compared to with chip used
chip_or_not <- dataset %>% select(fraud_flagged, chip_usage)
chip_or_not <- chip_or_not %>%
  group_by(fraud_flagged, chip_usage) %>%
  summarise(count = n())
print(chip_or_not)

# fraud ~3x more likely for international transfers
international_risk <- dataset %>% select(fraud_flagged, international_trans)
international_risk <- international_risk %>%
  group_by(fraud_flagged, international_trans) %>%
  summarise(count = n())
print(international_risk)

numerical <- dataset %>% select(fraud_flagged, risk_score, trans_value)

# risk score is indeed higher for the fraud transactions
risk_assessment_yes <- numerical %>%
  filter(fraud_flagged == "Yes") %>%
  ggplot() +
  geom_histogram(aes(x = risk_score))
print(risk_assessment_yes)
risk_assessment_no <- numerical %>%
  filter(fraud_flagged == "No") %>%
  ggplot() +
  geom_histogram(aes(x = risk_score))
print(risk_assessment_no)

# high value transactions seem more likely for fraud, but not convincingly higher
value_yes <- numerical %>%
  filter(fraud_flagged == "Yes") %>%
  ggplot() +
  geom_histogram(binwidth = 10, aes(x = trans_value)) +
  xlim(0, 4000)
mean_yes <- numerical %>%
  filter(fraud_flagged == "Yes") %>%
  summarise(mean_value = mean(trans_value))
print(mean_yes)
print(value_yes)
value_no <- numerical %>%
  filter(fraud_flagged == "No") %>%
  ggplot() +
  geom_histogram(binwidth = 10, aes(x = trans_value)) +
  xlim(0, 4000)
mean_no <- numerical %>%
  filter(fraud_flagged == "No") %>%
  summarise(mean_value = mean(trans_value))
print(mean_no)
print(value_no)

# 
pay_meths <- dataset %>% select(fraud_flagged, payment_method)
pay_meths <- pay_meths %>%
  group_by(fraud_flagged, payment_method) %>%
  summarise(count = n())
print(pay_meths)

# 2037 NaN values for transaction times of day, leaves 97963 rows filtered out
times <- dataset %>% select(fraud_flagged, month_date, trans_time)
times$trans_time <- substr(times$trans_time, start = 1, stop = 2)
times <- times %>%
  mutate(month = substr(month_date, start = 1, stop = 2),
         date = substr(month_date, start = 4, stop = 5)) %>%
  select(-month_date)
times <- times %>% filter(trans_time != "na")
times$trans_time <- as.numeric(times$trans_time)
times$month <- as.numeric(times$month)
times$date <- as.numeric(times$date)
print(times)

# more transactions occur during daytime, yet no indication that more daytime fraud occurs
yes_hr <- times %>%
  filter(fraud_flagged == "Yes") %>%
  ggplot(aes(x = trans_time)) +
  geom_histogram(binwidth = 1)
no_hr <- times %>%
  filter(fraud_flagged == "No") %>%
  ggplot(aes(x = trans_time)) +
  geom_histogram(binwidth = 1)
print(yes_hr)
print(no_hr)

# fraud appears to be bimodal by month with peaks in April and October/November
yes_month <- times %>%
  filter(fraud_flagged == "Yes") %>%
  ggplot(aes(x = month)) +
  geom_histogram(binwidth = 1)
no_month <- times %>%
  filter(fraud_flagged == "No") %>%
  ggplot(aes(x = month)) +
  geom_histogram(binwidth = 1)
print(yes_month)
print(no_month)

# no indication that fraud is more likely on some particular date of the month
yes_date <- times %>%
  filter(fraud_flagged == "Yes") %>%
  ggplot(aes(x = date)) +
  geom_histogram(binwidth = 1)
no_date <- times %>%
  filter(fraud_flagged == "No") %>%
  ggplot(aes(x = date)) +
  geom_histogram(binwidth = 1)
print(yes_date)
print(no_date)

# 43/183 cards responsible for multiple instances of fraud
yes_cards <- dataset %>%
  filter(fraud_flagged == "Yes") %>%
  select(card)
card_counts <- table(yes_cards$card)
cards_occuring_once <- names(card_counts)[card_counts == 1]
print(cards_occuring_once)
print(length(cards_occuring_once))
repeated_frauds <- length(yes_cards$card) - length(cards_occuring_once) 
print(repeated_frauds)