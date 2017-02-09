# Load the packages -------------------------------------------------------
library(checkpoint)
checkpoint("2017-01-15")
library(tidyverse)
library(forcats)
library(mice)
library(caret)
library(Metrics)
library(e1071)
library(doMC)
registerDoMC(cores = 4)
set.seed(29082012)

# dataset path
file_train <- "data/train.csv"
file_test <- "data/test.csv"


# Load the train & test dataset -------------------------------------------
columns <- cols(
  MSSubClass = col_factor(levels = c("20", "30", "40", "45", "50", "60", "70", "75", "80", "85", "90", "120", "150", "160", "180", "190")), # The building class
  # MSZoning = col_factor(), # The general zoning classification
  LotFrontage = col_integer(), # Linear feet of street connected to property
   LotArea = col_integer(), # Lot size in square feet
  # Street = col_factor(), # Type of road access
  # Alley = col_factor(levels = c("NA", "Pave", "Grvl")), # Type of alley access
  LotShape = col_factor(ordered = T, levels = c("IR3", "IR2", "IR1", "Reg")), # General shape of property
  # LandContour = col_factor(), # Flatness of the property
  Utilities = col_factor(ordered = T, levels = c("ELO", "NoSeWa", "NoSewr", "AllPub")), # Type of utilities available
  # LotConfig = col_factor(), # Lot configuration
  LandSlope = col_factor(ordered = T, levels = c("Sev", "Mod", "Gtl")), # Slope of property
  # Neighborhood = col_factor(), # Physical locations within Ames city limits
  Condition1 = col_factor(levels = c("RRAe", "RRNe", "PosA", "PosN", "RRAn", "RRNn", "Norm", "Feedr", "Artery")), # Proximity to main road or railroad
  Condition2 = col_factor(levels = c("RRAe", "RRNe", "PosA", "PosN", "RRAn", "RRNn", "Norm", "Feedr", "Artery")), # Proximity to main road or railroad (if a second is present)
  # BldgType = col_factor(), # Type of dwelling
  # HouseStyle = col_factor(), # Style of dwelling
  OverallQual = col_factor(ordered = T, levels = seq(1,10)), # Overall material and finish quality
  OverallCond = col_factor(ordered = T, levels = seq(1,10)), # Overall condition rating
  YearBuilt = col_integer(), # Original construction date
  YearRemodAdd = col_integer(), # Remodel date
  # RoofStyle = col_factor(), # Type of roof
  # RoofMatl = col_factor(), # Roof material
  # Exterior1st = col_factor(), # Exterior covering on house
  # Exterior2nd = col_factor(), # Exterior covering on house (if more than one material)
  # MasVnrType = col_factor(), # Masonry veneer type
  MasVnrArea = col_integer(), # Masonry veneer area in square feet
  ExterQual = col_factor(ordered = T, levels = c("Po", "Fa", "TA", "Gd", "Ex")), # Exterior material quality
  ExterCond = col_factor(ordered = T, levels = c("Po", "Fa", "TA", "Gd", "Ex")), # Present condition of the material on the exterior
  # Foundation = col_factor(), # Type of foundation
  # BsmtQual = col_factor(ordered = T, levels = c("NA", "Po", "Fa", "TA", "Gd", "Ex")), # Height of the basement
  # BsmtCond = col_factor(ordered = T, levels = c("NA", "Po", "Fa", "TA", "Gd", "Ex")), # General condition of the basement
  # BsmtExposure = col_factor(ordered = T, levels = c("NA", "No", "Mn", "Av", "Gd")), # Walkout or garden level basement walls
  # BsmtFinType1 = col_factor(ordered = T, levels = c("NA", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ")), # Quality of basement finished area
  BsmtFinSF1 = col_integer(), # Type 1 finished square feet
  # BsmtFinType2 = col_factor(ordered = T, levels = c("NA", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ")), # Quality of second finished area (if present)
  BsmtFinSF2 = col_integer(), # Type 2 finished square feet
  BsmtUnfSF = col_integer(), # Unfinished square feet of basement area
  TotalBsmtSF = col_integer(), # Total square feet of basement area
  # Heating = col_factor(), # Type of heating
  HeatingQC = col_factor(ordered = T, levels = c("Po", "Fa", "TA", "Gd", "Ex")), # Heating quality and condition
  # CentralAir = col_factor(), # Central air conditioning
  Electrical = col_factor(ordered = T, levels = c("Mix", "FuseP", "FuseF", "FuseA", "SBrkr")), # Electrical system
  `1stFlrSF` = col_integer(), # First Floor square feet
  `2ndFlrSF` = col_integer(), # Second floor square feet
  LowQualFinSF = col_integer(), # Low quality finished square feet (all floors)
  GrLivArea = col_integer(), # Above grade (ground) living area square feet
  BsmtFullBath = col_integer(), # Basement full bathrooms
  BsmtHalfBath = col_integer(), # Basement half bathrooms
  FullBath = col_integer(), # Full bathrooms above grade
  HalfBath = col_integer(), # Half baths above grade
  BedroomAbvGr = col_integer(), # Number of bedrooms above basement level
  KitchenAbvGr = col_integer(), # Number of kitchens
  KitchenQual = col_factor(ordered = T, levels = c("Po", "Fa", "TA", "Gd", "Ex")), # Kitchen quality
  TotRmsAbvGrd = col_integer(), # Total rooms above grade (does not include bathrooms)
  Functional = col_factor(ordered = T, levels = c("Sal", "Sev", "Maj2", "Maj1", "Mod", "Min2", "Min1", "Typ")), # Home functionality rating
  Fireplaces = col_integer(), # Number of fireplaces
  # FireplaceQu = col_factor(ordered = T, levels = c("NA", "Po", "Fa", "TA", "Gd", "Ex")), # Fireplace quality
  # GarageType = col_factor(levels = c("NA", "Detchd", "CarPort", "BuiltIn", "Basment", "Attchd", "2Types")), # Garage location
  GarageYrBlt = col_integer(), # Year garage was built
  # GarageFinish = col_factor(ordered = T, levels = c("NA", "Unf", "RFn", "Fin")), # Interior finish of the garage
  GarageCars = col_integer(), # Size of garage in car capacity
  GarageArea = col_integer(), # Size of garage in square feet
  # GarageQual = col_factor(ordered = T, levels = c("NA", "Po", "Fa", "TA", "Gd", "Ex")), # Garage quality
  # GarageCond = col_factor(ordered = T, levels = c("NA", "Po", "Fa", "TA", "Gd", "Ex")), # Garage condition
  PavedDrive = col_factor(ordered = T, levels = c("N", "P", "Y")), # Paved driveway
  WoodDeckSF = col_integer(), # Wood deck area in square feet
  OpenPorchSF = col_integer(), # Open porch area in square feet
  EnclosedPorch = col_integer(), # Enclosed porch area in square feet
  `3SsnPorch` = col_integer(), # Three season porch area in square feet
  ScreenPorch = col_integer(), # Screen porch area in square feet
  PoolArea = col_integer(), # Pool area in square feet
  # PoolQC = col_factor(ordered = T, levels = c("NA", "Fa", "TA", "Gd", "Ex")), # Pool quality
  # Fence = col_factor(ordered = T, levels = c("NA", "MnWw", "GdWo", "MnPrv", "GdPrv")), # Fence quality
  # MiscFeature = col_factor(levels = c("NA", "TenC", "Shed", "Othr", "Gar2", "Elev")), # Miscellaneous feature not covered in other categories
  MiscVal = col_integer(), # $Value of miscellaneous feature
  MoSold = col_integer(), # Month Sold
  YrSold = col_integer() # Year Sold
  # SaleType = col_factor(), # Type of sale
  # SaleCondition = col_factor() # Condition of sale
)

train_data <- read_csv(file_train, col_types = columns)
test_data <- read_csv(file_test, col_types = columns)

# I would recommend removing any houses with more than 4000 square feet from the data set
train_data <- train_data %>%
  filter(GrLivArea < 4000)

full_data <- bind_rows(train_data, test_data)

# Check character columns and turn them into factors-----------------------
glimpse(full_data)
summary(full_data)

full_data <- full_data %>%
  mutate(BsmtQual = ifelse(is.na(BsmtQual) == T, "None", BsmtQual)) %>%
  mutate(BsmtQual = parse_factor(BsmtQual, ordered = T, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"))) %>%
  
  mutate(Alley = ifelse(is.na(Alley) == T, "None", Alley)) %>%
  mutate(Alley = parse_factor(Alley, levels = c("None", "Pave", "Grvl"))) %>%
  
  mutate(BsmtCond = ifelse(is.na(BsmtCond) == T, "None", BsmtCond)) %>%
  mutate(BsmtCond = parse_factor(BsmtCond, ordered = T, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"))) %>%
  
  mutate(BsmtExposure = ifelse(is.na(BsmtExposure) == T, "None", BsmtExposure)) %>%
  mutate(BsmtExposure = parse_factor(BsmtExposure, ordered = T, levels = c("None", "No", "Mn", "Av", "Gd"))) %>%
  
  mutate(BsmtFinType1 = ifelse(is.na(BsmtFinType1) == T, "None", BsmtFinType1)) %>%
  mutate(BsmtFinType1 = parse_factor(BsmtFinType1, ordered = T, levels = c("None", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"))) %>%
  
  mutate(BsmtFinType2 = ifelse(is.na(BsmtFinType2) == T, "None", BsmtFinType2)) %>%
  mutate(BsmtFinType2 = parse_factor(BsmtFinType2, ordered = T, levels = c("None", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"))) %>%
  
  mutate(FireplaceQu = ifelse(is.na(FireplaceQu) == T, "None", FireplaceQu)) %>%
  mutate(FireplaceQu = parse_factor(FireplaceQu, ordered = T, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"))) %>%
  
  mutate(GarageType = ifelse(is.na(GarageType) == T, "None", GarageType)) %>%
  mutate(GarageType = parse_factor(GarageType, levels = c("None", "Detchd", "CarPort", "BuiltIn", "Basment", "Attchd", "2Types"))) %>%
  
  mutate(GarageFinish = ifelse(is.na(GarageFinish) == T, "None", GarageFinish)) %>%
  mutate(GarageFinish = parse_factor(GarageFinish, ordered = T, levels = c("None", "Unf", "RFn", "Fin"))) %>%
  
  mutate(GarageQual = ifelse(is.na(GarageQual) == T, "None", GarageQual)) %>%
  mutate(GarageQual = parse_factor(GarageQual, ordered = T, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"))) %>%
  
  mutate(GarageCond = ifelse(is.na(GarageCond) == T, "None", GarageCond)) %>%
  mutate(GarageCond = parse_factor(GarageCond, ordered = T, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"))) %>%
  
  mutate(PoolQC = ifelse(is.na(PoolQC) == T, "None", PoolQC)) %>%
  mutate(PoolQC = parse_factor(PoolQC, ordered = T, levels = c("None", "Fa", "TA", "Gd", "Ex"))) %>%
  
  mutate(Fence = ifelse(is.na(Fence) == T, "None", Fence)) %>%
  mutate(Fence = parse_factor(Fence, ordered = T, levels = c("None", "MnWw", "GdWo", "MnPrv", "GdPrv"))) %>%
  
  mutate(MiscFeature = ifelse(is.na(MiscFeature) == T, "None", MiscFeature)) %>%
  mutate(MiscFeature = parse_factor(MiscFeature, levels = c("None", "TenC", "Shed", "Othr", "Gar2", "Elev")))

full_data %>%  
  select_if(is_character) %>%
  summarise_each(funs(n_distinct(.))) %>%
  t() %>% data.frame() %>% as_tibble() %>% rownames_to_column() %>%
  arrange(desc(.))

full_data <- full_data %>%
  mutate_if(is_character, funs(as.factor))

# Look for NA's in full_data ----------------------------------------------
full_nas <- full_data %>%
  summarise_each(funs(sum(is.na(.)))) %>%
  t() %>% data.frame() %>% as_tibble() %>% rownames_to_column() %>%
  mutate(percent = ./nrow(full_data)*100) %>%
  arrange(desc(percent)) %>%
  print

# Remove columns with most NA's
#full_data  <- full_data %>%
#  select(-one_of(full_nas$rowname[1:4]), -one_of(full_nas$rowname[6]))

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

  
# Explore columns with small variance
full_data %>%
  select(nearZeroVar(full_data)) %>%
  colnames()

# Remove columns with near zero variance
full_data  <- full_data %>%
  select(-nearZeroVar(.))
  

# Continuous variables exploration ----------------------------------------
full_data %>%
  mutate(FirstFlrSF = `1stFlrSF`, SecondFlrSF = `2ndFlrSF`) %>%
  select(-`1stFlrSF`, -`2ndFlrSF`) %>%
  select_if(is.numeric) %>%
  summarise_each(funs(skewness(.)))

# Normalize skewed variables
full_data <- full_data %>%
  mutate(FirstFlrSF = `1stFlrSF`, SecondFlrSF = `2ndFlrSF`) %>%
  select(-`1stFlrSF`, -`2ndFlrSF`)# %>%

price <- full_data %>%
  select(SalePrice)

full_data <- full_data %>%
  select(-SalePrice) %>%
  mutate_if(function(col) is.numeric(col) && skewness(col) > 0.75, funs(log(. + 1))) %>%
  bind_cols(SalePrice = price)



# Resplit train and test data ---------------------------------------------

train_data <- full_data %>%
  filter(!is.na(SalePrice)) %>%
  mutate(logSalePrice = log(SalePrice + 1)) %>%
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
  number = 5,
  ## repeated ten times
  repeats = 5)

trGrid <-  expand.grid(.alpha = seq(0, .8, length.out = 50),
                       .lambda = seq(0, 0.1, length.out = 100))

glmFit<-train(logSalePrice~., data=select(training, -Id),
              method='glmnet',
              preProcess = c("center", "scale", "nzv"),
              trControl = fitControl,
              maximize=FALSE,
              metric = "RMSE",
              tuneGrid=trGrid
)
glmFit
ggplot(glmFit)
varImp(glmFit,scale=F)
mean(glmFit$resample$RMSE)

glm_predictions<- predict(glmFit, select(testing, -Id))
rmse(testing$logSalePrice, glm_predictions)

trGrid <-  expand.grid(.mtry = seq(0, 93, length.out = 6))

# ranger
rfFit<-train(logSalePrice~., data=select(training, -Id),
              method='ranger',
              preProcess = c("center", "scale", "nzv"),
              trControl = fitControl,
              maximize=FALSE,
              metric = "RMSE",
              tuneGrid=trGrid
)
rfFit

rf_predictions<- predict(rfFit, select(testing, -Id))
rmse(testing$logSalePrice, rf_predictions)

trGrid <-  expand.grid(.C = round(seq(1, 3, length.out = 3)),
                       .nprune = seq(1, 30, length.out = 10))

# non-linear gamSpline
svmFit<-train(logSalePrice~., data=select(training, -Id),
             method='svmLinear2',
             preProcess = c("center", "scale", "nzv"),
             trControl = fitControl,
             maximize=FALSE,
             metric = "RMSE",
             tuneLength = 5
)
svmFit
svm_predictions<- predict(svmFit, select(testing, -Id))
rmse(testing$logSalePrice, svm_predictions)

# randomGLM
rglmFit<-train(logSalePrice~., data=select(training, -Id),
              method='randomGLM',
              preProcess = c("center", "scale", "nzv"),
              trControl = fitControl,
              maximize=FALSE,
              metric = "RMSE",
              tuneLength = 2
)
rglmFit
rglm_predictions<- predict(rglmFit, select(testing, -Id))
rmse(testing$logSalePrice, rglm_predictions)

# Create model_list
model_list <- list(glm = glmFit, rf = rfFit)

# Pass model_list to resamples(): resamples
resamples <- resamples(model_list)

# Summarize the results
summary(resamples)
bwplot(resamples, metric = "RMSE")

mean_prediction <- rowMeans(cbind(glm_predictions, rf_predictions))
rmse(testing$logSalePrice, mean_prediction)


# Write submission file ---------------------------------------------------
glm_final <- predict(glmFit, newdata = select(test_data, -Id))
rf_final <- predict(rfFit, newdata = select(test_data, -Id))
sp_final <- predict(spFit, newdata = select(test_data, -Id))
Prediction <- glm_final#rowMeans(cbind(glm_final, rf_final))
submit <- data.frame(Id = test_data$Id, SalePrice = exp(Prediction) - 1)
write.csv(submit, file = "mysubmission.csv", row.names = FALSE)

