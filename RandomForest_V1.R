# Model
ranger_modelV1 <- ranger(SalePrice ~ ., data = train, num.trees = 500,
                       respect.unordered.factors = "order", 
                       seed = set.seed(1234))

# Prediction on train data
ranger_predictV1_train <- predict(ranger_modelV1, train)
train_pred$RandomForest <- ranger_predictV1_train$predictions

# RMSE and RSquared
train_pred %>% mutate(residualRNGR = SalePrice - RandomForest) %>% 
  summarise(rmse = sqrt(mean(residualRNGR ^ 2)), 
            r2 = 1 - (sum(residualRNGR ^ 2) / 
                        sum((SalePrice - mean(SalePrice)) ^ 2)))

# R2 1- RMSE 2- Rsquared 3- Mean Absolute Error (MAE)
caret::postResample(train_pred$SalePrice, train_pred$RandomForest)[2]

# Add R2 and RMSE to train_rmse_r2
train_rmse_r2$RMSE[train_rmse_r2$Algorithm == "Random Forest"] <- 
  sqrt(mean((train_pred$SalePrice - train_pred$RandomForest) ^ 2))

train_rmse_r2$RSquared[train_rmse_r2$Algorithm == "Random Forest"] <- 
  1 - (sum((train_pred$SalePrice - train_pred$RandomForest) ^ 2) / 
         sum((train_pred$SalePrice - mean(train_pred$SalePrice)) ^ 2))

# Plot prediction vs actual
ggplot(train_pred, aes(x = RandomForest, y = SalePrice)) + 
  geom_point() + 
  geom_abline()

# Prediction on test data
ranger_predictV1_test <- predict(ranger_modelV1, test)
test_pred$RandomForest <- ranger_predictV1_test$predictions
