# Model
ranger_modelV1 <- ranger(SalePrice ~ ., data = train, num.trees = 500,
                       respect.unordered.factors = "order", 
                       seed = set.seed(1234))

# Prediction on train data
ranger_predictV1_train <- predict(ranger_modelV1, train)
train$pred_rng_train <- ranger_predictV1_train$predictions

# RMSE
train %>% mutate(residualRNGR = SalePrice - pred_rng_train) %>% 
  summarise(rmse = sqrt(mean(residualRNGR ^ 2)), 
            r2 = 1 - (sum(residualRNGR ^ 2) / 
                        sum((SalePrice - mean(SalePrice)) ^ 2)))

# R2 1- RMSE 2- Rsquared 3- Mean Absolute Error (MAE)
caret::postResample(train$SalePrice, pred_rng_train)[2]

# Plot prediction vs actual
ggplot(test_rngr, aes(x = pred_rng_train, y = SalePrice)) + 
  geom_point() + 
  geom_abline()

# Prediction on test data
ranger_predictV1_test <- predict(ranger_modelV1, test)
pred_rngr <- ranger_predictV1_test$predictions
