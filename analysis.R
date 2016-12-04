# Load the packages -------------------------------------------------------
library(checkpoint)
checkpoint("2016-11-15")
library(tidyverse)

library(doMC)
registerDoMC(cores = 8)
set.seed(29082012)

# dataset path
file_train <- "data/train.csv"
file_test <- "data/test.csv"


# Load the train & test dataset -------------------------------------------
column_types <- cols_only(a = col_factor())

train_data <- read_csv(file_train, col_types = column_types)
test_data <- read_csv(file_test, col_types = column_types)

full_data = bind_rows(train_data, test_data)

# Look for NA's in full_data ----------------------------------------------
full_data %>%
  summarise_each(funs(sum(is.na(.)))) %>%
  print
