# Tidy recipe for custom model cross-validation with custom metric

library(titanic)
library(tidyverse)
library(rsample)
library(rBayesianOptimization)
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
  fit <- rpart::rpart(Survived ~ ., fit_train,
                      control = rpart::rpart.control(cp = params[["cp"]],
                                                     minsplit = params[["minsplit"]]))
}


# custom validating function ----------------------------------------

validating <- function(fit, fit_validate){
  pred <- predict(fit, fit_validate)
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
random_grid <- expand.grid(cp = seq(0.0001, 0.01, 0.001),
                         minsplit = seq(10, 100, 10)) %>%
  as_tibble() %>%
  # sample_frac(0.1) %>% # random grid search
  mutate(param = split(., seq(nrow(.)))) %>%
  mutate(result = map(param, evaluate_param)) %>%
  unnest(result)

# random_grid %>%
#   top_n(5, result)

random_grid %>%
  mutate(id = c(1:nrow(random_grid))) %>%
  ggplot(aes(x = id, y = result)) +
  geom_point() +
  geom_smooth() +
  labs(title = paste("Best score: ", max(random_grid$result)))


# Bayesian

evaluate_param_bayes <- function(cp, minsplit){
  params <- list(cp = cp,
                minsplit = minsplit)

  cv_data <- cross_validating(data_train, fitting, validating, params) %>%
    mutate(validate_prediction = map2(model, cv_validate, predict))

  Score <- mean(cv_data$validation)
  Pred <- flatten_dbl(cv_data$validate_prediction) %>% as.numeric()

  return(list(Score = Score, Pred = Pred))
}

bayes <- BayesianOptimization(evaluate_param_bayes,
                              bounds = list(
                                cp = c(0.0001, 0.01),
                                minsplit = c(10L, 100L)
                              ),
                              init_points = 10,
                              n_iter = 20)

bayes$History %>%
  ggplot(aes(x = Round, y = Value)) +
  geom_point() +
  geom_smooth() +
  labs(title = paste("Best score:", max(bayes$History$Value)))


# Test: anderes Modell
# Test: Ensembling mit mehr params
