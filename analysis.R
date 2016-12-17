# Load the packages -------------------------------------------------------
library(checkpoint)
checkpoint("2016-11-15")
library(tidyverse)
library(forcats)
library(mice)
library(caret)
library(Metrics)
library(doMC)
registerDoMC(cores = 8)
set.seed(29082012)

# dataset path
file_train <- "data/train.csv"
file_test <- "data/test.csv"


# Load the train & test dataset -------------------------------------------
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
full_nas <- full_data %>%
  summarise_each(funs(sum(is.na(.)))) %>%
  t() %>% data.frame() %>% as_tibble() %>% rownames_to_column() %>%
  mutate(percent = ./nrow(full_data)*100) %>%
  arrange(desc(percent)) %>%
  print

# Remove columns with most NA's
full_data  <- full_data %>%
  select(-one_of(full_nas$rowname[1:4]), -one_of(full_nas$rowname[6]))

# Explore columns with small variance
full_data %>%
  select(nearZeroVar(.)) %>%
  colnames()

# Remove columns with near zero variance
full_data  <- full_data %>%
  select(-nearZeroVar(.))


# Plotting time -----------------------------------------------------------
#  Target var distribution
full_data %>%
  mutate(logSalePrice = log(SalePrice)) %>%
  select(logSalePrice) %>%
  ggplot(aes(x = logSalePrice)) +
    geom_histogram()

full_data %>%
  mutate(logSalePrice = log(SalePrice)) %>%
  select(logSalePrice, YearBuilt) %>%
  ggplot(aes(x = YearBuilt, y = logSalePrice)) +
  geom_jitter() +
  geom_smooth()

# Impute missing variables ------------------------------------------------
if(!file.exists("imputed.csv")) {
  imputed_data <- complete(mice(select(full_data, -SalePrice), meth = c("cart")))
  write_csv(imputed_data, path = "imputed.csv", col_names = T)
} else {
  imputed_data <- read.csv("imputed.csv", header = T)
  imputed_data <- tbl_df(imputed_data)
}

nas <- full_nas %>%
  filter(. > 0, rowname != "SalePrice") %>%
  select(rowname)

imputed_nas <- imputed_data %>%
  select(one_of(nas$rowname))

full_data <- full_data %>%
  select(-one_of(nas$rowname)) %>%
  bind_cols(imputed_nas)

full_data %>%
  summarise_each(funs(sum(is.na(.)))) %>%
  print

# Resplit train and test data ---------------------------------------------

train_data <- full_data %>%
  filter(!is.na(SalePrice)) %>%
  mutate(logSalePrice = log(SalePrice)) %>%
  select(-SalePrice)

test_data <- full_data %>%
  filter(is.na(SalePrice)) %>%
  select(-SalePrice)


# Model fitting and validation --------------------------------------------
inTrain <- createDataPartition(train_data$logSalePrice, p = 0.75, list = F)

training <- train_data %>%
  slice(inTrain)
testing <- train_data %>%
  slice(-inTrain)

# glmnet -------------------------------------------------------------------
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10,
  savePredictions="final")

trGrid <-  expand.grid(.alpha = seq(0, 0.2, length.out = 40),
                       .lambda = seq(0, 1, length.out = 10))

glmFit<-train(logSalePrice~., data=select(training, -Id),
              method='glmnet',
              trControl = fitControl,
              metric = "RMSE",
              tuneGrid=trGrid
)
glmFit
ggplot(glmFit)
varImp(glmFit,scale=F)

glm_predictions<- predict(glmFit, select(testing, -Id))
rmse(testing$logSalePrice, glm_predictions)

# ranger
rfFit<-train(logSalePrice~., data=select(training, -Id),
              method='ranger',
              trControl = fitControl,
              metric = "RMSE",
              tuneLength = 3
)
rfFit

rf_predictions<- predict(rfFit, select(testing, -Id))
rmse(testing$logSalePrice, rf_predictions)

trGrid <-  expand.grid(.degree = round(seq(1, 3, length.out = 3)),
                       .nprune = seq(1, 30, length.out = 10))

# non-linear gamSpline
spFit<-train(logSalePrice~., data=select(training, -Id),
             method='earth',
             trControl = fitControl,
             metric = "RMSE",
             tuneGrid=trGrid
)
spFit
sp_predictions<- predict(spFit, select(testing, -Id))
rmse(testing$logSalePrice, sp_predictions)

# Create model_list
model_list <- list(glm = glmFit, rf = rfFit, mars = spFit)

# Pass model_list to resamples(): resamples
resamples <- resamples(model_list)

# Summarize the results
summary(resamples)
bwplot(resamples, metric = "RMSE")

mean_prediction <- rowMeans(cbind(glm_predictions, rf_predictions, sp_predictions))
rmse(testing$logSalePrice, mean_prediction)


# Write submission file ---------------------------------------------------
glm_final <- predict(glmFit, newdata = select(test_data, -Id))
rf_final <- predict(rfFit, newdata = select(test_data, -Id))
sp_final <- predict(spFit, newdata = select(test_data, -Id))
Prediction <- rowMeans(cbind(glm_final, rf_final, sp_final))
submit <- data.frame(Id = test_data$Id, SalePrice = exp(Prediction))
write.csv(submit, file = "mysubmission.csv", row.names = FALSE)

