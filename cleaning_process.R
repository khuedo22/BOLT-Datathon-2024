library(readxl)
library(stringr)

# read in the initial dataset
dataset <- read_excel("BOLT_dataset.xlsx")

# only keep month and date of transaction since year is the same
dataset$'Transaction Date' <- substr(dataset$'Transaction Date', start = 6, stop = 10)

# function to reformat transaction times to hh:mm:ss
rearrange_time <- function(time) {
  # keep the NaN values
  if (time == 'na:n:') {
    return(time)
  }
  
  # remove all non-numeric characters (. and :)
  time <- gsub("[^0-9]", "", time)
  
  # delete the .0 from 6-digit times down to 5 digits
  if (nchar(time) <= 6) {
    time <- substr(time, start = 1, stop = nchar(time) - 1)
  }
  
  # truncate valid 7-digit times down to 6 digits
  if (nchar(time) == 7) {
    time <- substr(time, start = 1, stop = 6)
  }
  
  # pad the leftmost digits with zeroes
  if (nchar(time) <= 5) {
    time <- str_pad(time, width = 6, side = "left", pad = "0")
  }
  
  # rearrange to hh:mm:ss and return the time
  time <- paste0(substr(time, 1, 2), ":", substr(time, 3, 4), ":", substr(time, 5, 6))
  return (time)
}

# apply the function to reformat transaction times to hh:mm:ss
dataset$'Transaction Time' <- sapply(dataset$'Transaction Time', rearrange_time)

# give new, shorter names to the columns
new_column_names <- c("card", "month_date", "trans_time", "risk_score", "payment_method", "trans_value", "merchant_country", "card_present", "chip_usage", "international_trans", "acquirer", "merchant", "MCC", "fraud_flagged")
names(dataset) <- new_column_names

# write the dataset to a new excel file
clean_dataset <- "clean_BOLT_dataset.csv"
write.csv(dataset, file = clean_dataset, row.names = FALSE)