process_data <- function(X, strata = NULL) {
  
  # impute the covariates with missingness
  p_missing <- sapply(X, function(x) mean(is.na(x)))
  
  # nodes that are already complete
  no_missing <- names(p_missing[p_missing == 0])
  processed <- X[, no_missing, with = FALSE]
  
  # nodes to impute
  to_impute <- names(p_missing[(0 < p_missing)])
  if (length(to_impute) > 0) {
    missing_indicators <- X[, lapply(.SD, function(x) as.numeric(!is.na(x))),
                            .SDcols = to_impute
                            ]
    missing_names <- sprintf("delta_%s", to_impute)
    setnames(missing_indicators, missing_names)
    if(is.null(strata)){
      imputed <- X[,lapply(.SD, impute_by_type), .SDcols=to_impute]
    } else {
      imputed <- X[,lapply(.SD, impute_by_type), .SDcols=to_impute, by=strata]
    }
    processed <- cbind(processed, imputed, missing_indicators)
  } 
  return(processed)
}

impute_median <- function(x) {
  value <- median(as.numeric(x[!is.na(x)]))
  x[is.na(x)] <- value
  x
}

impute_mode <- function(x) {
  count_df <- aggregate(count ~ x, data = data.frame(count = 1, x = x), sum)
  value <- count_df$x[which.max(count_df$count)]
  x[is.na(x)] <- value
  return(x)
}

impute_by_type <- function(x) {
  if (is.factor(x) || is.character(x)) {
    return(impute_mode(x))
  } else {
    return(impute_median(x))
  }
}
