
# sketch ------------------------------------------------------------------



# data %>%
#   add_data %>%
#   transform_data(PIPELINE, DATA, FUN) %>% #in: data, preproc fun; out: preprocessed data --> add to named list
#   add_train_test_split(PIPELINE, DATA) %>% #in: data; out: list of 2 datasets --> add to named list
#   add_cv_splits(PIPELINE, DATA) %>% #in: (training) data; out: validation split indices --> add to named list
#   add_fitting(FUN) %>% #--> add to named list, data in, model out
#   add_evaluation(FUN) %>% #--> add to named list, data and model in, scalar out
#   tune_bayesian(DATA = train, #set default
#                 VALIDATION = cv_idx, #set default
#                 FITTING = fit_fun, #set default
#                 EVAL = eval_fun, #set default
#                 PARAM = bounds,
#                 ITER) %>% #out: bayes tuning object
#   add_model(PARAMS) %>% #in: best params, out: model --> add to named list
#   add_test_score(MODEL, TEST_DATA)
#
# #for stacking, add oof predictions
# #for ensembling, fit different models on the same pipeline with different fitting fun or params



# setup -------------------------------------------------------------------

library(titanic)
library(tidyverse)
library(rlang)
library(xgboost)
set.seed(42)

# data I/O ----------------------------------------------------------------

data <- titanic_train %>%
  select(-PassengerId)

data %<>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.factor, as.numeric)

data$Survived <- titanic_train$Survived


# add data ----------------------------------------------------------------

add_data <- function(data){
  return(list(data = data))
}


# transformer -------------------------------------------------------------

#TODO transformer function fitted to train and applied to test

# train/test split --------------------------------------------------------

add_train_test_split <- function(p, data){
  # data <- enexpr(data) #works both if data is passed as character and unquoted
  data <- as_name(enquo(data))
  intrain <- rsample::initial_split(p[[data]])$in_id
  p$train_data <- p[[data]][intrain, ]
  p$test_data <- p[[data]][-intrain, ]
  return(p)
}

#TODO: add stratification

# cv splits ---------------------------------------------------------------

add_cv_splits <- function(p, train_data, k){
  # train_data <- enexpr(train_data) #works both if data is passed as character and unquoted
  train_data <- as_name(enquo(train_data))
  sp <- rsample::vfold_cv(p[[train_data]], k)
  in_idx <- map(sp$splits, function(x){x$in_id})
  out_idx <- map(in_idx, function(x){setdiff(c(1:nrow(p[[train_data]])), x)})
  p$cv_train_idx <- in_idx
  p$cv_val_idx <- out_idx
  return(p)
}

#TODO: add stratification


# fitting function --------------------------------------------------------

fitting <- function(data, target, ...){ # ... specify hyperparameters
  target <- as_name(enquo(target)) # works if target is name or symbol
  if(!target %in% colnames(data)) stop("Target not in data")

  dtrain <- xgb.DMatrix(data.matrix(data[-match(target, colnames(data))]),
                        label = data[[target]])

  params <- list(...)

  if(is.null(params$nrounds)) stop("No nrounds specified in function call")

  fit <- xgb.train(nrounds = params$nrounds,
                   data = dtrain,
                   params = params)
}

# f <- fitting(data, Survived, eta = 0.2, nrounds = 10, gamma = 2)

add_fitting_fun <- function(p, fun, target){

  target <- as_name(enquo(target)) # works if target is name or symbol

  # utility function to set default args for arbitrary function
  hijack <- function (FUN, ...) {
    .FUN <- FUN
    args <- list(...)
    invisible(lapply(seq_along(args), function(i) {
      formals(.FUN)[[names(args)[i]]] <<- args[[i]]
    }))
    .FUN
  }

  # modifies fitting function to set target as default
  fun <- hijack(fun, target = target)

  p$fitting_fun <- fun
  return(p)
}


# evaluation --------------------------------------------------------------

#TODO


# apply fitting to cv -----------------------------------------------------

cv_eval_params <- function(p, ...){
  fun <- function(idx){
    p$fitting_fun(p$train_data[idx, ], ...)
  }

  map(p$cv_train_idx, fun)
}

# cv <- cv_eval_params(p, nrounds = 10, gamma = 2)
# str(cv)

#TODO what's the best way to deal with the target variable?

# main pipeline -----------------------------------------------------------

p <- data %>%
  add_data() %>%
  add_train_test_split(data) %>%
  add_cv_splits(train_data, 5) %>%
  add_fitting_fun(fitting, Survived) %>%
  cv_eval_params(nrounds = 10, gamma = 2)

str(p)
