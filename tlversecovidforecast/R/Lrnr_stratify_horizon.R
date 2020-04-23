#' Bound Predictions
#'
#' This learner bounds predictions. Intended for use in a pipeline.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#'
#' @export
#'
#' @keywords data
#'
#' @return Learner object with methods for training and prediction. See
#'  \code{\link{Lrnr_base}} for documentation on learners.
#'
#' @format \code{\link{R6Class}} object.
#'
#' @family Learners
#'
#' @section Parameters:
#' \describe{
#'   \item{\code{bound = .005}}{Either a length two vector of c(lower,upper) or a
#'   lower bound, where the upper is then 1 - lower}
#'   \item{\code{...}}{Not currently used.}
#' }
#'
#' @template common_parameters
#
Lrnr_stratify_horizon <- R6Class(
  classname = "Lrnr_stratify_horizon",
  inherit = Lrnr_base, portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(transform, inverse_transform, learner, ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
      private$.name = sprintf("%s_transformed", learner$name)
    }
  ),
  private = list(
    .properties = c(
      "continuous", "binomial", "categorical", "weights"
    ),

    .make_trans_task = function(task){
      trans_dt <- data.table(outcome_trans=self$params$transform(task$Y))
      new_columns <- task$add_columns(trans_dt)
      trans_task <- task$next_in_chain(column_names=new_columns, 
                                       outcome = "outcome_trans")
      
      return(trans_task)
    },
    .train = function(task) {
      trans_task <- private$.make_trans_task(task)
      fit <- self$params$learner$train(trans_task)
      
      fit_object <- self$params
      fit_object$learner <- fit
      
      return(fit_object)
    },

    .predict = function(task = NULL) {
      trans_task <- private$.make_trans_task(task)
      preds <- self$fit_object$learner$predict(trans_task)
      trans_preds <- self$params$inverse_transform(preds)
      
      return(trans_preds)
      
    },
    .name = "transform"
  )
)
