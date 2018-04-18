
library(tidyverse)

train_full <- read.csv("train.csv")
dim(train_full)

# Check NAs 
na_count <- sapply(train_full, function(x) sum(length(which(is.na(x)))))
na_count <- data.frame(na_count)

# Data imputation
train_full$LotFrontage[is.na(train_full$LotFrontage)] <- round(
  mean(train_full$LotFrontage, na.rm = TRUE))

train_full$Alley <- factor(train_full$Alley, 
                           levels = levels(addNA(train_full$Alley)), 
                           labels = c(levels(train_full$Alley), "Not Available"),
                           exclude = NULL)

train_full$MasVnrType <- `levels<-`(addNA(train_full$MasVnrType), 
                                    c(levels(train_full$MasVnrType), 
                                      "None"))

train_full$MasVnrArea[is.na(train_full$MasVnrArea)] <- round(
  mean(train_full$MasVnrArea, na.rm = TRUE))

train_full$BsmtQual <- `levels<-`(addNA(train_full$BsmtQual), 
                                  c(levels(train_full$BsmtQual), 
                                    "No Basement"))

train_full$BsmtCond <- `levels<-`(addNA(train_full$BsmtCond), 
                                  c(levels(train_full$BsmtCond), 
                                    "No Basement"))

train_full$BsmtExposure <- `levels<-`(addNA(train_full$BsmtExposure), 
                                      c(levels(train_full$BsmtExposure), 
                                        "No Basement"))

train_full$BsmtFinType1 <- `levels<-`(addNA(train_full$BsmtFinType1), 
                                      c(levels(train_full$BsmtFinType1), 
                                        "No Basement"))

train_full$BsmtFinType2 <- `levels<-`(addNA(train_full$BsmtFinType2), 
                                      c(levels(train_full$BsmtFinType2), 
                                        "No Basement"))

train_full$Electrical <- `levels<-`(addNA(train_full$Electrical), 
                                      c(levels(train_full$Electrical), 
                                        "SBrkr"))

train_full$FireplaceQu <- `levels<-`(addNA(train_full$FireplaceQu), 
                                c(levels(train_full$FireplaceQu), 
                                  "No Fireplace"))

train_full$GarageType <- `levels<-`(addNA(train_full$GarageType), 
                                   c(levels(train_full$GarageType), 
                                     "No Garage"))

train_full$GarageYrBlt[is.na(train_full$GarageYrBlt)] <- "No Garage"

train_full$GarageFinish <- `levels<-`(addNA(train_full$GarageFinish), 
                                      c(levels(train_full$GarageFinish), 
                                        "No Garage"))

train_full$GarageQual <- `levels<-`(addNA(train_full$GarageQual), 
                                    c(levels(train_full$GarageQual), 
                                      "No Garage"))

train_full$GarageCond <- `levels<-`(addNA(train_full$GarageCond), 
                                    c(levels(train_full$GarageCond), 
                                      "No Garage"))

train_full$PoolQC <- `levels<-`(addNA(train_full$PoolQC), 
                                c(levels(train_full$PoolQC), 
                                  "No Pool"))

train_full$Fence <- `levels<-`(addNA(train_full$Fence), 
                               c(levels(train_full$Fence), 
                                 "No Fence"))

train_full$MiscFeature <- `levels<-`(addNA(train_full$MiscFeature), 
                                     c(levels(train_full$MiscFeature), 
                                       "None"))


# Shuffle and split train_full
smp_size <- floor(.7 * nrow(train_full))

set.seed(123)
train_ind <- sample(seq_len(nrow(train_full)), size = smp_size)

train <- train_full[train_ind, ]
test <- train_full[-train_ind, ]


