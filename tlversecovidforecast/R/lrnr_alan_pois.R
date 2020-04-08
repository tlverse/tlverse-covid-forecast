#' Generalized Linear Models
#'
#' This learner provides fitting procedures for generalized linear models using
#' \code{\link[stats]{glm.fit}}.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom stats glm predict family
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
#'   \item{\code{...}}{Parameters passed to \code{\link[stats]{glm}}.}
#' }
#'
#' @template common_parameters
#
Lrnr_alan_pois <- R6Class(
  classname = "Lrnr_alan_pois", inherit = Lrnr_base,
  portable = TRUE, class = TRUE,
  public = list(
    initialize = function(intercept = TRUE, ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    }
  ),
  
  private = list(
    .properties = c("continuous", "binomial", "weights", "offset"),
    .task_dt = function(task){
      dt <- data.table(log_cases= task$Y, 
                       elapse = task$get_node("time"), 
                       region = task$id)
      return(dt)
    },
    .generate_basis = function(dt){
      dt[,cases:=exp(log_cases)-1]
      dt[,newcases:=c(0,diff(cases)),by=list(region)]
      dt[newcases<0,newcases:=0] #remove any decreases in counts
      dt[,elapse2:=elapse^2]
      dt[,elapse3:=elapse^3]
      
      basis <- list(y=dt$newcases, x= as.matrix(dt[,list(elapse, elapse2, elapse3)]))
      return(basis)
      
    },
    
    .train = function(task) {
      # Alan's fit
      # glm.Pois = glm(newcases~elapse+elapse2+elapse3,family = poisson(),data=tst)
      dt <- private$.task_dt(task)
      args <- private$.generate_basis(dt)
      args$family <- poisson()
      
      family_name <- args$family$family
      linkinv_fun <- args$family$linkinv
      link_fun <- args$family$linkfun
      
      
      
      fit_object <- sl3:::call_with_args(stats::glm.fit, args)
     
      fit_object$linear.predictors <- NULL
      fit_object$weights <- NULL
      fit_object$prior.weights <- NULL
      fit_object$y <- NULL
      fit_object$residuals <- NULL
      fit_object$fitted.values <- NULL
      fit_object$effects <- NULL
      fit_object$qr <- NULL
      fit_object$linkinv_fun <- linkinv_fun
      fit_object$link_fun <- link_fun
      fit_object$training_offset <- task$has_node("offset")
      return(fit_object)
    },
    .predict = function(task) {
      
      # for each region we need predictions from 1:maxdata
      
      dt <- private$.task_dt(task)
      newdata_max <- dt[,list(elapse=max(elapse)),by=list(region)]
      new_data <- newdata_max[,list(elapse=seq(from=0, to=elapse),log_cases=1),by=list(region)]
      basis <- private$.generate_basis(new_data)
      
      fit_object <- self$fit_object
      coef <- fit_object$coef
      pred_raw <- basis$x%*%coef
      
      new_data$pred_newcases <- fit_object$linkinv_fun(pred_raw)
      new_data[,pred_cases:=cumsum(pred_newcases), by=list(region)]
      new_data[,pred_logcases:=log(pred_cases+1)]
      pred_data <- merge(dt[,list(region,elapse)], new_data, by=c("region", "elapse"), sort=FALSE, all.x=TRUE)
      predictions <- pred_data$pred_logcases
      return(predictions)
    }
  )
)
