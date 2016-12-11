# Load the packages -------------------------------------------------------
library(checkpoint)
checkpoint("2016-11-15")
library(tidyverse)
library(forcats)
library(caret)
library(doMC)
registerDoMC(cores = 8)
set.seed(29082012)

# dataset path
file_train <- "data/train.csv"
file_test <- "data/test.csv"


# Load the train & test dataset -------------------------------------------
column_types <- cols_only()

train_data <- read_csv(file_train)
test_data <- read_csv(file_test)

full_data = bind_rows(train_data, test_data)

# Check character columns and turn them into factors-----------------------
full_data %>%  
  select_if(is_character) %>%
  summarise_each(funs(n_distinct(.))) %>%
  t() %>% data.frame() %>% as_tibble() %>% rownames_to_column() %>%
  arrange(desc(.))

full_data <- full_data %>%
  mutate_if(is_character, funs(as.factor))

glimpse(full_data)
summary(full_data)
# Look for NA's in full_data ----------------------------------------------
full_data %>%
  summarise_each(funs(sum(is.na(.)))) %>%
  t() %>% data.frame() %>% as_tibble() %>% rownames_to_column() %>%
  mutate(percent = ./nrow(full_data)*100) %>%
  arrange(desc(percent))

# Explore columns with small variance
full_data %>%
  select(nearZeroVar(.))









