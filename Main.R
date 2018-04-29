
library(tidyverse)
library(ranger)
library(xgboost)
library(vtreat)
library(magrittr)

train <- read.csv("train.csv")
dim(train)

# Check NAs 
na_count <- sapply(train, function(x) sum(length(which(is.na(x)))))
na_count <- data.frame(na_count)

# Train Data imputation
train$LotFrontage[is.na(train$LotFrontage)] <- round(
  mean(train$LotFrontage, na.rm = TRUE))

train$Alley <- factor(train$Alley, 
                           levels = levels(addNA(train$Alley)), 
                           labels = c(levels(train$Alley), "Not Available"),
                           exclude = NULL)

train$MasVnrType <- `levels<-`(addNA(train$MasVnrType), 
                                    c(levels(train$MasVnrType), 
                                      "None"))

train$MasVnrArea[is.na(train$MasVnrArea)] <- round(
  mean(train$MasVnrArea, na.rm = TRUE))

train$BsmtQual <- `levels<-`(addNA(train$BsmtQual), 
                                  c(levels(train$BsmtQual), 
                                    "No Basement"))

train$BsmtCond <- `levels<-`(addNA(train$BsmtCond), 
                                  c(levels(train$BsmtCond), 
                                    "No Basement"))

train$BsmtExposure <- `levels<-`(addNA(train$BsmtExposure), 
                                      c(levels(train$BsmtExposure), 
                                        "No Basement"))

train$BsmtFinType1 <- `levels<-`(addNA(train$BsmtFinType1), 
                                      c(levels(train$BsmtFinType1), 
                                        "No Basement"))

train$BsmtFinType2 <- `levels<-`(addNA(train$BsmtFinType2), 
                                      c(levels(train$BsmtFinType2), 
                                        "No Basement"))

train$Electrical <- `levels<-`(addNA(train$Electrical), 
                                      c(levels(train$Electrical), 
                                        "SBrkr"))

train$FireplaceQu <- `levels<-`(addNA(train$FireplaceQu), 
                                c(levels(train$FireplaceQu), 
                                  "No Fireplace"))

train$GarageType <- `levels<-`(addNA(train$GarageType), 
                                   c(levels(train$GarageType), 
                                     "No Garage"))

train$GarageYrBlt[is.na(train$GarageYrBlt)] <- "No Garage"

train$GarageFinish <- `levels<-`(addNA(train$GarageFinish), 
                                      c(levels(train$GarageFinish), 
                                        "No Garage"))

train$GarageQual <- `levels<-`(addNA(train$GarageQual), 
                                    c(levels(train$GarageQual), 
                                      "No Garage"))

train$GarageCond <- `levels<-`(addNA(train$GarageCond), 
                                    c(levels(train$GarageCond), 
                                      "No Garage"))

train$PoolQC <- `levels<-`(addNA(train$PoolQC), 
                                c(levels(train$PoolQC), 
                                  "No Pool"))

train$Fence <- `levels<-`(addNA(train$Fence), 
                               c(levels(train$Fence), 
                                 "No Fence"))

train$MiscFeature <- `levels<-`(addNA(train$MiscFeature), 
                                     c(levels(train$MiscFeature), 
                                       "None"))

# Working on test data
test <- read.csv("test.csv")

# Check NAs 
na_count_test <- sapply(test, function(x) sum(length(which(is.na(x)))))
na_count_test <- data.frame(na_count_test)

# Test Data imputation
test$MSZoning <- `levels<-`(addNA(test$MSZoning), 
                              c(levels(test$MSZoning), 
                                "RL"))

test$LotFrontage[is.na(test$LotFrontage)] <- round(
  mean(test$LotFrontage, na.rm = TRUE))

test$Alley <- factor(test$Alley, 
                           levels = levels(addNA(test$Alley)), 
                           labels = c(levels(test$Alley), "Not Available"),
                           exclude = NULL)

test$Utilities <- `levels<-`(addNA(test$Utilities), 
                            c(levels(test$Utilities), 
                              "AllPub"))

test$Exterior1st <- `levels<-`(addNA(test$Exterior1st), 
                              c(levels(test$Exterior1st), 
                                "VinylSd"))

test$Exterior2nd <- `levels<-`(addNA(test$Exterior2nd), 
                               c(levels(test$Exterior2nd), 
                                 "VinylSd"))

test$MasVnrType <- `levels<-`(addNA(test$MasVnrType), 
                                    c(levels(test$MasVnrType), 
                                      "None"))

test$MasVnrArea[is.na(test$MasVnrArea)] <- round(
  mean(test$MasVnrArea, na.rm = TRUE))

test$BsmtQual <- `levels<-`(addNA(test$BsmtQual), 
                                  c(levels(test$BsmtQual), 
                                    "No Basement"))

test$BsmtCond <- `levels<-`(addNA(test$BsmtCond), 
                                  c(levels(test$BsmtCond), 
                                    "No Basement"))

test$BsmtExposure <- `levels<-`(addNA(test$BsmtExposure), 
                                      c(levels(test$BsmtExposure), 
                                        "No Basement"))

test$BsmtFinType1 <- `levels<-`(addNA(test$BsmtFinType1), 
                                      c(levels(test$BsmtFinType1), 
                                        "No Basement"))

test$BsmtFinType2 <- `levels<-`(addNA(test$BsmtFinType2), 
                                      c(levels(test$BsmtFinType2), 
                                        "No Basement"))

test$BsmtFinSF1[is.na(test$BsmtFinSF1)] <- as.integer(round(
  mean(test$BsmtFinSF1, na.rm = TRUE)))

test$BsmtFinSF2[is.na(test$BsmtFinSF2)] <- as.integer(round(
  median(test$BsmtFinSF2, na.rm = TRUE)))

test$BsmtUnfSF[is.na(test$BsmtUnfSF)] <- as.integer(round(
  median(test$BsmtUnfSF, na.rm = TRUE)))

test$TotalBsmtSF[is.na(test$TotalBsmtSF)] <- as.integer(round(
  mean(test$TotalBsmtSF, na.rm = TRUE)))

test$Electrical <- `levels<-`(addNA(test$Electrical), 
                                    c(levels(test$Electrical), 
                                      "SBrkr"))

test$BsmtFullBath[is.na(test$BsmtFullBath)] <- as.integer(round(
  mean(test$BsmtFullBath, na.rm = TRUE)))

test$BsmtHalfBath[is.na(test$BsmtHalfBath)] <- as.integer(0)

test$KitchenQual <- `levels<-`(addNA(test$KitchenQual), 
                              c(levels(test$KitchenQual), 
                                "TA"))

test$Functional <- `levels<-`(addNA(test$Functional), 
                               c(levels(test$Functional), 
                                 "Typ"))

test$FireplaceQu <- `levels<-`(addNA(test$FireplaceQu), 
                               c(levels(test$FireplaceQu), 
                                 "No Fireplace"))

test$GarageType <- `levels<-`(addNA(test$GarageType), 
                              c(levels(test$GarageType), 
                                "No Garage"))

test$GarageYrBlt[is.na(test$GarageYrBlt)] <- "No Garage"

test$GarageFinish <- `levels<-`(addNA(test$GarageFinish), 
                                c(levels(test$GarageFinish), 
                                  "No Garage"))

test$GarageCars[is.na(test$GarageCars)] <- as.integer(round(
  median(test$GarageCars, na.rm = TRUE)))

test$GarageArea[is.na(test$GarageArea)] <- as.integer(0)


test$GarageQual <- `levels<-`(addNA(test$GarageQual), 
                                    c(levels(test$GarageQual), 
                                      "No Garage"))

test$GarageCond <- `levels<-`(addNA(test$GarageCond), 
                                    c(levels(test$GarageCond), 
                                      "No Garage"))

test$PoolQC <- `levels<-`(addNA(test$PoolQC), 
                                c(levels(test$PoolQC), 
                                  "No Pool"))

test$Fence <- `levels<-`(addNA(test$Fence), 
                               c(levels(test$Fence), 
                                 "No Fence"))

test$MiscFeature <- `levels<-`(addNA(test$MiscFeature), 
                                     c(levels(test$MiscFeature), 
                                       "None"))

test$SaleType <- `levels<-`(addNA(test$SaleType), 
                              c(levels(test$SaleType), 
                                "WD"))

# Train result
train_result <- data.frame(Algorithm = c("Random Forest", "XGBZ", "XGBN"),
                           RMSE      = vector("numeric", 3),
                           RSquared  = vector("numeric", 3))

# Train result RMSE R2
train_rmse_r2 <- data.frame(Algorithm = c("Random Forest", "XGBZ", "XGBN"),
                           RMSE      = vector("numeric", 3),
                           RSquared  = vector("numeric", 3))

# Train predictions
train_pred <- data.frame(SalePrice    = train$SalePrice,
                         RandomForest = vector("numeric", nrow(train)),
                         XGBZ         = vector("numeric", nrow(train)),
                         XGBN         = vector("numeric", nrow(train)))


# Test predictions
test_pred <- data.frame(Id           = test$Id,
                       RandomForest = vector("numeric", nrow(test)),
                       XGBZ         = vector("numeric", nrow(test)),
                       XGBN         = vector("numeric", nrow(test)))


# Result
prediction <- test_pred %>% select(Id, XGBZ)
names(prediction)[names(prediction) == "XGBZ"] <- "SalePrice"
write.csv(prediction, file = "prediction.csv", row.names = FALSE)
