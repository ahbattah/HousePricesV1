# Variables without predictor
vars <- names(train)[-length(names(train))]

# First: Using designTreatmentsZ
set.seed(1234)
train_treatZ <- designTreatmentsZ(train, vars)

(scoreFrameZ <- train_treatZ %>%
    use_series(scoreFrameZ) %>%
    select(varName, origName, code))

(newvarsZ <- scoreFrameZ %>%
    filter(code %in% c("clean", "lev")) %>%
    use_series(varName))

(train_prepZ <- prepare(train_treatZ, train, varRestriction = newvarsZ))

(test_prepZ <- prepare(train_treatZ, test, varRestriction = newvarsZ))

set.seed(1234)
cv_z <- xgb.cv(data = as.matrix(train_prepZ), label = train$SalePrice,
               nrounds = 100, nfold = 5, objective = "reg:linear",
               eta = 0.3, max_depth = 10, early_stopping_rounds = 10,
               verbose = 0)

elog_z <- cv_z$evaluation_log

elog_z %>% 
  summarize(ntrees.train = which.min(train_rmse_mean), 
            ntrees.test  = which.min(test_rmse_mean))

# Model
model_xgb_z <- xgboost(data = as.matrix(train_prepZ), 
                       label = train$SalePrice,
                       nrounds = 49, objective = "reg:linear",
                       eta = 0.3, depth = 6, vervose = 0)

# Prediction on train data
train$predZ <- predict(model_xgb_z, as.matrix(train_treatZ))

# RMSE
train %>% mutate(residualXGB_Z = SalePrice - predZ) %>% 
  summarise(rmse = sqrt(mean(residualXGB_Z ^ 2)),
            r2 = 1 - (sum(residualXGB_Z ^ 2) / 
                        sum((SalePrice - mean(SalePrice)) ^ 2)))

# R2 1- RMSE 2- Rsquared 3- Mean Absolute Error (MAE)
caret::postResample(train$SalePrice, test$predZ)[2]

# Plot prediction vs actual
ggplot(train, aes(x = predZ, y = SalePrice)) + 
  geom_point() + 
  geom_abline()

# Prediction on test data
pred_z <- predict(model_xgb_z, as.matrix(test_prepZ))


# =======================================================================
# Second: Using designTreatmentsN
set.seed(1234)
train_treatN <- designTreatmentsN(train, vars, "SalePrice")

(scoreFrameN <- train_treatN %>%
    use_series(scoreFrameN) %>%
    select(varName, origName, code))

(newvarsN <- scoreFrameN %>%
    filter(code %in% c("clean", "lev", "catN")) %>%
    use_series(varName))

(train_prepN <- prepare(train_treatN, train, varRestriction = newvarsN))

(test_prepN <- prepare(train_treatN, test, varRestriction = newvarsN))

set.seed(1234)
cv_n <- xgb.cv(data = as.matrix(train_prepN), label = train$SalePrice,
               nrounds = 100, nfold = 5, objective = "reg:linear",
               eta = 0.3, max_depth = 10, early_stopping_rounds = 10,
               verbose = 0)

elog_n <- cv_n$evaluation_log

elog_n %>% 
  summarize(ntrees.train = which.min(train_rmse_mean), 
            ntrees.test  = which.min(test_rmse_mean))

# Model
model_xgb_n <- xgboost(data = as.matrix(train_prepN), 
                       label = train$SalePrice,
                       nrounds = 49, objective = "reg:linear",
                       eta = 0.3, depth = 6, vervose = 0)

# Prediction on train data
train$predN <- predict(model_xgb_n, as.matrix(train_treatN))


# RMSE
test %>% mutate(residualXGB_N = SalePrice - predN) %>% 
  summarise(rmse = sqrt(mean(residualXGB_N ^ 2)))

# R2 1- RMSE 2- Rsquared 3- Mean Absolute Error (MAE)
caret::postResample(test$SalePrice, test$predN)[2]

# Plot prediction vs actual
ggplot(test, aes(x = predN, y = SalePrice)) + 
  geom_point() + 
  geom_abline()

# Prediction on test data
predN <- predict(model_xgb_n, as.matrix(test_prepN))
