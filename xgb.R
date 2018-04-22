set.seed(1234)

# Split data
train_indexs <- caret::createDataPartition(train_full$SalePrice, p = .7, 
                                           list = F)
train_xgb <- train_full[train_indexs, ]
test_xgb  <- train_full[-train_indexs, ]

# Variables without predictor
vars <- names(train_xgb)[-length(names(train_xgb))]

# First: Using designTreatmentsZ
train_xgb_treatZ <- designTreatmentsZ(train_xgb, vars)

(scoreFrameZ <- train_xgb_treatZ %>%
    use_series(scoreFrameZ) %>%
    select(varName, origName, code))

(newvarsZ <- scoreFrameZ %>%
    filter(code %in% c("clean", "lev")) %>%
    use_series(varName))

(train_prepZ <- prepare(train_xgb_treatZ, train_xgb, varRestriction = newvarsZ))

(test_prepZ <- prepare(train_xgb_treatZ, test_xgb, varRestriction = newvarsZ))

cv_z <- xgb.cv(data = as.matrix(train_prepZ), label = train_xgb$SalePrice,
               nrounds = 100, nfold = 5, objective = "reg:linear",
               eta = 0.3, max_depth = 10, early_stopping_rounds = 10,
               verbose = 0)

elog_z <- cv_z$evaluation_log

elog_z %>% 
  summarize(ntrees.train = which.min(train_rmse_mean), 
            ntrees.test  = which.min(test_rmse_mean))

# Model
model_xgb_z <- xgboost(data = as.matrix(train_prepZ), 
                       label = train_xgb$SalePrice,
                       nrounds = 49, objective = "reg:linear",
                       eta = 0.3, depth = 6, vervose = 0)

# Prediction
test_xgb$predZ <- predict(model_xgb_z, as.matrix(test_prepZ))


# Mean
cat("Mean actual values (Z) = ", mean(test_xgb$SalePrice), 
    "\nMean predicted values = ", mean(test_xgb$predZ))

# RMSE
test_xgb %>% mutate(residualXGB_Z = SalePrice - predZ) %>% 
  summarise(rmse = sqrt(mean(residualXGB_Z ^ 2)))

# R2 1- RMSE 2- Rsquared 3- Mean Absolute Error (MAE)
caret::postResample(test_xgb$SalePrice, test_xgb$predZ)[2]

# Plot prediction vs actual
ggplot(test_xgb, aes(x = predZ, y = SalePrice)) + 
  geom_point() + 
  geom_abline()


# =======================================================================
# Second: Using designTreatmentsN
set.seed(1234)
train_xgb_treatN <- designTreatmentsN(train_xgb, vars, "SalePrice")

(scoreFrameN <- train_xgb_treatN %>%
    use_series(scoreFrameN) %>%
    select(varName, origName, code))

(newvarsN <- scoreFrameN %>%
    filter(code %in% c("clean", "lev")) %>%
    use_series(varName))

(train_prepN <- prepare(train_xgb_treatN, train_xgb, varRestriction = newvarsN))

(test_prepN <- prepare(train_xgb_treatN, test_xgb, varRestriction = newvarsN))

cv_n <- xgb.cv(data = as.matrix(train_prepN), label = train_xgb$SalePrice,
               nrounds = 100, nfold = 5, objective = "reg:linear",
               eta = 0.3, max_depth = 10, early_stopping_rounds = 10,
               verbose = 0)

elog_n <- cv_n$evaluation_log

elog_n %>% 
  summarize(ntrees.train = which.min(train_rmse_mean), 
            ntrees.test  = which.min(test_rmse_mean))

# Model
model_xgb_n <- xgboost(data = as.matrix(train_prepN), 
                       label = train_xgb$SalePrice,
                       nrounds = 49, objective = "reg:linear",
                       eta = 0.3, depth = 6, vervose = 0)

# Prediction
test_xgb$predN <- predict(model_xgb_n, as.matrix(test_prepN))


# Mean
cat("Mean actual values (N) = ", mean(test_xgb$SalePrice), 
    "\nMean predicted values = ", mean(test_xgb$predN))

# RMSE
test_xgb %>% mutate(residualXGB_N = SalePrice - predN) %>% 
  summarise(rmse = sqrt(mean(residualXGB_N ^ 2)))

# R2 1- RMSE 2- Rsquared 3- Mean Absolute Error (MAE)
caret::postResample(test_xgb$SalePrice, test_xgb$predN)[2]

# Plot prediction vs actual
ggplot(test_xgb, aes(x = predN, y = SalePrice)) + 
  geom_point() + 
  geom_abline()

