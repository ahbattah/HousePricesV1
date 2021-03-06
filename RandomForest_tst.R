# Shuffle and split train
sample_size <- floor(.7 * nrow(train))

set.seed(123)
train_ind <- sample(seq_len(nrow(train)), size = sample_size)

train_rngr <- train[train_ind, ]
test_rngr <- train[-train_ind, ]

# Model
ranger_model <- ranger(SalePrice ~ ., data = train_rngr, num.trees = 500,
                       respect.unordered.factors = "order", 
                       seed = set.seed(1234))
# Prediction
ranger_predict <- predict(ranger_model, test_rngr)
test_rngr$pred <- ranger_predict$predictions

# Mean
cat("Mean actual values = ", mean(test_rngr$SalePrice), 
    "\nMean predicted values = ", mean(test_rngr$pred))

# RMSE
test_rngr %>% mutate(residualRNGR = SalePrice - pred) %>% 
  summarise(rmse = sqrt(mean(residualRNGR ^ 2)),
            r2 = 1 - (sum(residualRNGR ^ 2) / 
                        sum((SalePrice - mean(SalePrice)) ^ 2)))

# R2 1- RMSE 2- Rsquared 3- Mean Absolute Error (MAE)
caret::postResample(test_rngr$SalePrice, test_rngr$pred)[2]

# Add R2 and RMSE to train_result
train_result$RMSE[train_result$Algorithm == "Random Forest"] <- 
  sqrt(mean((test_rngr$SalePrice - test_rngr$pred) ^ 2))

train_result$RSquared[train_result$Algorithm == "Random Forest"] <- 
  1 - (sum((test_rngr$SalePrice - test_rngr$pred) ^ 2) / 
         sum((test_rngr$SalePrice - mean(test_rngr$SalePrice)) ^ 2))
  
# Plot prediction vs actual
ggplot(test_rngr, aes(x = log(pred), y = log(SalePrice))) + 
  geom_point() + 
  geom_abline()

