## extract stability paths from refits of <model> on resampled <data>.
## arguments:
##    model: a fitted model object of class "regsubsets"
##    data: the data used for the model fit
##    method: resampling method, either "subsample" (without replacement) or
##      "bootstrap" (with replacement)
##    strata: for subsampling, vector (length = nrow(data)) defining the strata
##      for stratified sampling.
##    fraction: subsampling fraction
## return: a <max. subsetsize + 1> x <covariates> matrix with relative selection
##    frequencies. first row is the null model (no covariates,i.e., all 0s)
## dependencies: {checkmate}, {leaps}
get_stability_paths <- function(model, data, reps = 100,
                                method = c("subsample", "bootstrap"),
                                strata = NULL, fraction = 0.5) {
  checkmate::assert_class(model, "regsubsets")
  checkmate::assert_data_frame(data)
  checkmate::assert_count(reps)
  method <- match.arg(method)
  checkmate::assert_vector(strata, any.missing = FALSE,
                           len = NROW(data), null.ok = TRUE)
  checkmate::assert_number(fraction, lower = 0, upper = 1)

  selected <- vector("list", reps)
  for (i in seq_len(reps)) {
    new_data <- resample(data, method = method, strata = strata,
                         fraction = fraction)
    new_model <- refit(model, new_data)
    selected[[i]] <- get_selected(new_model)
  }
  stability_paths <- make_paths(selected)
  stability_paths
}

############## resample ########################################################

resample <- function(data, method = c("subsample", "bootstrap"),
                     strata = NULL, fraction = 0.5) {
  nrows <- nrow(data)
  rows <- resample_rows(nrows, method, strata, fraction)
  data[rows, ]
}

resample_rows <- function(nrows, method, strata = NULL, fraction = 0.5) {
  switch(method,
         "bootstrap" = sample_with_replacement(nrows, strata),
         "subsample" = sample_without_replacement(nrows, strata,
                                                  fraction = fraction)
  )
}

sample_with_replacement <- function(nrows, strata = NULL) {
  if (is.null(strata)) {
    return(sample(1:nrows, replace = TRUE)) # --> early exit!
  }
  rows <- tapply(
    X = seq_len(nrows), INDEX = strata, FUN = sample, replace = TRUE
  )
  as.vector(rows)
}

sample_without_replacement <- function(nrows, strata = NULL, fraction = 0.5) {
  if (is.null(strata)) {
    # round up the sample size
    return(sample(1:nrows, size = ceiling(fraction * nrows), replace = FALSE)) # --> early exit!
  }
  # define rows that belong to different strata (factor = strata)
  index_by_strata <- split(1:nrows, strata)
  # define sizes for each strata
  size_by_strata <- ceiling(fraction * table(strata))
  # use mapply() as to sample from each 'index_by_strata', then unlist
  rows <- mapply(FUN = sample, x = index_by_strata, size = size_by_strata, 
                 replace = FALSE)
  unlist(rows)
}


############## refit ###########################################################

# redo subset selection <model> on <new_data>1
refit <- function(model, new_data) {
  # works by overwriting the data argument of the original model
  # and then re-doing the function call that produced the original model
  modelcall <- model$call
  modelcall$data <- new_data
  # use regsubsets-generic here instead of regsubsets.formula or other method as
  # these methods are not exported by {leaps}
  # (quote s.t. just the name of the function is handed over, not the
  # function code itself...)
  modelcall[[1]] <- quote(leaps::regsubsets)
  eval(modelcall)
}


############## selected ########################################################

get_selected <- function(new_model) {
  selected <- summary(new_model)[["which"]]
  # add 'zero-model' in the first row
  selected <- rbind("0" = rep(FALSE, ncol(selected)), selected)
  # delete the intercept-column, as we are only interested in variables
  selected[, -1]
}

############## paths ###########################################################

make_paths <- function(selected) {
  # found this solution on stackoverflow -> is this a good way?
  # at least, it's efficient to code
  
  # calculate averages for each position element wise
  Reduce(`+`, selected) / length(selected)
}
