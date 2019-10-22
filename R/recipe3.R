
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
  if(is.null(data)) stop("Training data not specified")
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

  # check if fun has arguments data, target and ...
  check_param <- any(!c("data", "target") %in% names(formals(fitting)))
  if(check_param) stop("function should have parameters: data, target")
  check_dots <- !"..." %in% names(formals(fitting))
  if(check_dots) warning("function doesn't have ... for hyperparam tuning")

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

#TODO check that fitting fun returns model

# validation --------------------------------------------------------------

# model = p$fitting_fun(p$train_data[p$cv_train_idx[[1]], ], eta = 0.2, nrounds = 10, gamma = 2)
# val_data = p$train_data[p$cv_val_idx[[1]], ]

validation <- function(val_data, model){
  feature_cols <- match(model$feature_names, colnames(val_data))
  dval <- xgb.DMatrix(data.matrix(val_data[, feature_cols]))
  pred <- predict(model, dval)
  obs <- val_data[, -feature_cols]
  auc <- MLmetrics::AUC(pred, obs)
  return(list(score = auc, pred = pred))
}

#TODO check that val fun has the right arguments
add_validation_fun <- function(p, fun){
  p$validation_fun <- fun
  return(p)
}

# apply fitting to cv -----------------------------------------------------

#TODO check if p has all necessary elements
#TODO add all p elements as parameters, set defaults for convenience

do_cv <- function(p, train_data = train_data, ...){
  train_data <- as_name(enquo(train_data))
  fun <- function(train_idx, val_idx){
    cv_model <- p$fitting_fun(p[[train_data]][train_idx, ], ...)
    result <- p$validation_fun(p[[train_data]][val_idx, ], cv_model)
    return(result)
  }

  cv_scores <- pmap(.l = list(train_idx = p$cv_train_idx, val_idx = p$cv_val_idx), .f = fun)
  cv_score_mean <- mean(flatten_dbl(pluck(transpose(cv_scores), "score")))
  p$cv_result <- list(cv_score_mean = cv_score_mean, params = list(...))
  return(p$cv_result)
}

# cv <- do_cv(p, nrounds = 30, gamma = 2, eta = 0.1)
# str(cv)


# grid search -------------------------------------------------------------

grid <- expand.grid(
  nrounds = c(10, 20),
  eta = c(0.1, 0.2),
  gamma = c(1, 2)
)

#TODO remove attributes for res_tbl

do_gridsearch <- function(p, train_data = train_data, grid, fraction = 1){
  if(fraction <= 0 | fraction > 1){stop("fraction is not in (0, 1]")}

  grid_sample <- dplyr::sample_frac(grid, fraction)
  grid_list <- map(c(1:nrow(grid_sample)), function(x){as.list(grid_sample[x, ])})

  res <- map(grid_list, function(x) do.call(do_cv, append(x, list(p = p))))
  res_tbl <- grid_sample
  res_tbl$cv_score_mean <- flatten_dbl(transpose(res)$cv_score_mean)
  p$gridsearch_result <- res_tbl
  return(p)
}


# bayesian optimization ---------------------------------------------------

bounds <- list(
  nrounds = c(10L, 20L),
  eta = c(0.1, 0.2),
  gamma = c(1, 2)
)

#TODO check that bounds is of type list

# do_bayesian_optim <- function(p, train_data = train_data, bounds){
#
#
#
#
# }

# main pipeline -----------------------------------------------------------

p <- data %>%
  add_data() %>%
  add_train_test_split(data) %>%
  add_cv_splits(train_data, 5) %>%
  add_fitting_fun(fitting, Survived) %>%
  add_validation_fun(validation) %>%
  do_gridsearch(grid = grid, fraction = 0.5)

str(p)

