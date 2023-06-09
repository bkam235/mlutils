% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/encode_target.R
\name{encode_target}
\alias{encode_target}
\title{Target encoding of features}
\usage{
encode_target(data, val_data = NULL,
  vars_to_encode = colnames(data)[-match(target_var, colnames(data))],
  target_var, n_folds = 5, alpha = 0.1)
}
\arguments{
\item{data}{data.table with features and target variable}

\item{val_data}{data.table of validation data with features}

\item{vars_to_encode}{features to encode}

\item{target_var}{target variable to encode features with}

\item{n_folds}{number of folds across which encodings are calculated}

\item{alpha}{smoothing parameter for encoding}
}
\description{
Target encoding of features
}
