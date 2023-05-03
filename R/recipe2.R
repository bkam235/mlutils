# Tidy recipe for custom model cross-validation with custom metric

library(titanic)
library(tidyverse)
library(rsample)
library(rBayesianOptimization)
library(xgboost)
set.seed(2019)


# data I/O ----------------------------------------------------------------

data <- titanic_train %>%
  select(-PassengerId)

data %<>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.factor, as.numeric)

data$Survived <- titanic_train$Survived

# validation setup --------------------------------------------------------

splits <- initial_split(data, 0.75, strata = Survived)
data_train <- training(splits)
data_test <- testing(splits)

# custom model fitting function -------------------------------------------

fitting <- function(fit_train, params){
  dtrain <- fit_train %>%
    select(-Survived) %>%
    data.matrix()


  fit <- xgb.train(params = params,
                   nround = params[["nrounds"]],
                   data = xgb.DMatrix(dtrain, label = fit_train$Survived))
}

# custom validating function ----------------------------------------

validating <- function(fit, fit_validate){
  dvalidate <- fit_validate %>%
    select(-Survived) %>%
    data.matrix() %>%
    xgb.DMatrix()

  pred <- predict(fit, dvalidate)
  pred <- ifelse(pred > 0.5, 1, 0)
  obs <- fit_validate$Survived
  MLmetrics::Accuracy(pred, obs)
}


# CV ----------------------------------------------------------------------

cross_validating <- function(data_fun,
                             fitting_fun,
                             validating_fun,
                             params){
  cv_splits <- vfold_cv(data_fun, v = 5, strata = Survived)

  cv_data <- cv_splits %>%
    mutate(cv_train = map(splits, training),
           cv_validate = map(splits, testing))

  cv_data <- cv_data %>%
    mutate(model = map(cv_train, fitting_fun, params = params),
           validation = map2_dbl(model, cv_validate, validating_fun))
}

# Tuning ------------------------------------------------------------------

evaluate_param <- function(params){

  result <- cross_validating(data_train, fitting, validating, params) %>%
    summarise(validation = mean(validation)) %>%
    pull(validation)

  return(result)
}

# grid search
random_grid <- expand.grid(eta = seq(0.001, 0.2, length.out = 5),
                           nrounds = round(seq(10, 100, length.out = 5)),
                           max_depth = round(seq(1, 15, length.out = 5)),
                           subsample = seq(0.1, 1, length.out = 5)) %>%
  as_tibble() %>%
  sample_n(30) %>% # random grid search
  mutate(param = split(., seq(nrow(.)))) %>%
  mutate(result = map(param, evaluate_param)) %>%
  unnest(result)

random_grid %>%
  mutate(Round = c(1:nrow(random_grid))) %>%
  ggplot(aes(x = Round, y = result)) +
  geom_point() +
  geom_smooth() +
  labs(title = paste("Best score: ", max(random_grid$result)),
       subtitle = c("random grid search"))

#Bayes
evaluate_param_bayes <- function(eta,
                                 nrounds,
                                 max_depth,
                                 subsample){
  params <- list(eta = eta,
                 nrounds = nrounds,
                 max_depth = max_depth,
                 subsample = subsample)

  cv_data <- cross_validating(data_train, fitting, validating, params) %>%
    mutate(cv_validate = map(cv_validate, function(x){
      x %>%
        select(-Survived) %>%
        data.matrix() %>%
        xgb.DMatrix()
      })) %>%
    mutate(validate_prediction = map2(model, cv_validate, predict))

  Score <- mean(cv_data$validation)
  Pred <- flatten_dbl(cv_data$validate_prediction) %>% as.numeric()

  return(list(Score = Score, Pred = Pred))
}

bayes <- BayesianOptimization(evaluate_param_bayes,
                              bounds = list(
                                eta = c(0.001, 0.2),
                                nrounds = c(10L, 100L),
                                max_depth = c(1L, 15L),
                                subsample = c(0.1, 1)
                              ),
                              init_points = 10,
                              n_iter = 20)

bayes$History %>%
  ggplot(aes(x = Round, y = Value)) +
  geom_point() +
  geom_smooth() +
  labs(title = paste("Best score:", max(bayes$History$Value)),
       subtitle = c("Bayesian optimization"))
