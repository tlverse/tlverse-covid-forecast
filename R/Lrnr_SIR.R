#' SIR
#'
#' This learner solves a simple SIR model
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
Lrnr_SIR <- R6Class(
  classname = "Lrnr_SIR", inherit = Lrnr_base,
  portable = TRUE, class = TRUE, lock_objects = FALSE,
  public = list(
    initialize = function(start = c(0.5, 0.5), ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    },
    SIR = function(time, state, parameters, N) {
      par <- as.list(c(state, parameters))
      with(par, {
        dS <- -beta/N * I * S
        dI <- beta/N * I * S - gamma * I
        dR <- gamma * I
        list(c(dS, dI, dR))
      })
    }
  ),
  private = list(
    .properties = c("continuous", "mechanistic"),
    .init = NULL,
    .task_dt = function(task, subset=TRUE){
      Y <- exp(task$Y)-1
      if(subset){
        data <- data.table(row=seq(nrow(task$data)),
                           region = task$get_node("id"),
                           Y=Y)
        inter<-data[, .SD[.N], by=region]
        inter <- inter[inter$Y==0, row]
      
        start_infection <- (Y>0)
        start_infection[inter] <- TRUE

        dt <- data.table(infected = Y[start_infection], 
                         elapse = task$get_node("time")[start_infection], 
                         region = task$id[start_infection],
                         population_size = task$get_node("population_size")[start_infection]
        )
        
      }else{
        dt <- data.table(infected = Y, 
                         elapse = task$get_node("time"), 
                         region = droplevels(task$id),
                         population_size = task$get_node("population_size")
                         ) 
      }
      return(dt)
    },

    .train = function(task) {
      
      start <- self$params$start
      dt <- private$.task_dt(task)
      dt$region <- factor(dt$region, 
                          levels=unique(as.character(dt$region))) #Needs to be reordered for split

      region_init <- dt[,list(elapse=as.numeric(max(elapse)-min(elapse)+1), 
                              start=min(elapse), 
                              N=as.numeric(mean(population_size)),
                              S=as.numeric(mean(population_size)-min(infected)),
                              I=as.numeric(min(infected)),
                              R=0),by=list(region)]
      region_init <- split(region_init, f = region_init$region)
      
      fit_object <- lapply(region_init, function(init){
        #ODE solver can't deal with single values!
        if(init$elapse==1){init$elapse<-2}
        dt_subset <- dt[dt$region==init$region,]
        RSS <- function(parameters) {
          names(parameters) <- c("beta", "gamma")
          #Initial state for the ODE; time for which output is wanted;
          out <- ode(y = c(S = init$S, I = init$I, R = init$R), 
                     times = seq(init$elapse), func = self$SIR, parms = parameters, 
                     N=init$N)
          fit <- out[ , 3]
          sum((dt_subset$infected - fit)^2)
        }
        Opt <- optim(c(0.5,0.5), RSS, method = "L-BFGS-B", 
                     lower = c(0, 0), upper = c(1, 1))
        setNames(Opt$par, c("beta", "gamma"))
      })
      
      return(fit_object)
    },
    .predict = function(task) {
      
      #Issue: this learner has problems with 0 counts
      #       should we backtrack?
      
      dt <- private$.task_dt(task)
      dt_full <- private$.task_dt(task, subset = FALSE)
      dt$region <- factor(dt$region, 
                          levels=unique(as.character(dt$region))) #Needs to be reordered for split
      region_init <- dt[,list(elapse=as.numeric(max(elapse)-min(elapse)+1), 
                              start=min(elapse), 
                              N=as.numeric(mean(population_size)),
                              S=as.numeric(mean(population_size)-min(infected)),
                              I=as.numeric(min(infected)),
                              R=0),by=list(region)]
      region_init <- split(region_init, f = as.factor(region_init$region), sorted=FALSE)
      
      preds <- lapply(seq(length(region_init)), function(i){
        init <- region_init[[i]]
        if(init$elapse==1){
          init$elapse<-2
          init$start<-init$start-1}
        fit_object <- self$fit_object[[i]]
        dt_subset <- dt_full[region==init$region,]
        pred <- data.frame(ode(y = c(S = init$S, I = init$I, R = init$R), 
                               times = seq(1:init$elapse), N=init$N,
                               func = self$SIR, parms = c(fit_object[1], fit_object[2])))
        matrix(c(rep(0, init$start), pred$I))
      })
      pred <- do.call(rbind, preds)
      
      #Transform back to log?
      pred_log <- log(pred+1)
      return(pred_log)
    },
    .required_packages = c("deSolve")
  )
)