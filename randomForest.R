# Shuffle and split train_full
sample_size <- floor(.7 * nrow(train_full))

set.seed(123)
train_ind <- sample(seq_len(nrow(train_full)), size = sample_size)

train_rngr <- train_full[train_ind, ]
test_rngr <- train_full[-train_ind, ]

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
  summarise(rmse = sqrt(mean(residualRNGR ^ 2)))

# R2 1- RMSE 2- Rsquared 3- Mean Absolute Error (MAE)
caret::postResample(test_rngr$SalePrice, test_rngr$pred)[2]

# Plot prediction vs actual
ggplot(test_rngr, aes(x = pred, y = SalePrice)) + 
  geom_point() + 
  geom_abline()

