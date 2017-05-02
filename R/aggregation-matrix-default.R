#' Aggregation.matrix.default
#'
#' This class represents the default aggregation matrix. This matrix was defined
#' by ... # TODO(Cosenza)
#'
#'
#' @export
#'
setClass(
  "Aggregation.matrix.default",
  representation( ),
  contains = "Aggregation.matrix",
  validity = function(object) {
    TRUE
  }

)



setMethod(
  f = "initialize",
  signature = "Aggregation.matrix.default",
  definition = function(.Object)
  {
    # cat("~~~ Aggregation.matrix.default: initializator ~~~ \n")
    # Assignment of the slots
    methods::validObject(.Object)
    return(.Object)
    # return of the object
  }
)


#' @rdname AggregateMatrix
#'
#'
#'
#' @export
#'
#' @return data.frame
#'
#' @include aggregation-matrix.R
#'
# Function to handle the solving of the agregate matrix. It call agregate for
# Project(j,i) and option(k,i), i vary from 1:number.of.factors
# This function needs a well behavored set of parameters, said: no NA, all
# values already checked and valid, and all project and options evaluated.
setMethod(
  "AggregateMatrix",
  signature = c(
    "Aggregation.matrix.default",
    "data.frame",
    "data.frame",
    "data.frame"),
  function(
    aggregation.matrix,
    project.portfolio.as.data.frame,
    project.portfolio.specifics.as.data.frame,
    option.portfolio.as.data.frame){

    # function body
    result <-
      data.frame(
        matrix(
          ncol = length(row.names(option.portfolio.as.data.frame)),
          nrow = length(row.names(project.portfolio.as.data.frame))
        )
      )

    colnames(result) <- row.names(option.portfolio.as.data.frame)
    rownames(result) <- row.names(project.portfolio.as.data.frame)

    nrfactors <- length(colnames(project.portfolio.as.data.frame))
    for (i in 1:length(row.names(project.portfolio.as.data.frame))) {
      for (j in 1:length(row.names(option.portfolio.as.data.frame))) {
        temp.list.agregation <- lapply(1:nrfactors, function(x)
          (Agregate(project.portfolio.as.data.frame[i, x],
                    option.portfolio.as.data.frame[j, x],
                    project.portfolio.specifics.as.data.frame[i, x],
                    nrfactors)))
        agregation <- NULL
        if (any(temp.list.agregation == -1)) agregation <- "out"
        else agregation <- sum(unlist(temp.list.agregation))
        result[i,j] <- agregation
      }
    }

    return(result)
  }
  )


#' Agregate
#'
#' This function do not validate entries, since it is not exported and the data
#' is validated by the constructors. The validation here would be resource
#' consuming.
#'
#' @param factor.evaluation character factor evaluation from project
#' @param resource.evaluation character factor evaluation from option
#' @param factor.is.specific logic indicates that this factor is specific for
#' the project
#' @param nrfactors numeric number of factors evaluated for each project/option
#'
#' @return numeric indicate the result factor per option. If a specific factor
#' is not achived it returns -1
#'
#'
Agregate <-
  function(
    factor.evaluation,
    resource.evaluation,
    factor.is.specific,
    nrfactors){
    # factor.evaluation  - character
    # resource.evaluation - character
    # factor.is.specific - logical
    # nrfactors - numeric
    if (factor.evaluation == "Cr") {
      if (resource.evaluation == "Excelent") return(1)
      if (factor.is.specific) return(-1)
      return(0) # if -> "Good", "Regular", "Weak", "Void", "Zero", "Inexistent
    }

    if (factor.evaluation == "C") {
      if (resource.evaluation == "Excelent") return(1 + 1/nrfactors)
      if (resource.evaluation == "Good") return(1)
      if (factor.is.specific) return(-1)
      return(0) # "Regular", "Weak", "Void", "Zero", "Inexistent"
    }

    if (factor.evaluation == "LC") {
      if (resource.evaluation == "Excelent") return(1 + 2/nrfactors)
      if (resource.evaluation == "Good") return(1 + 1/nrfactors)
      if (resource.evaluation == "Regular") return(1)
      if (factor.is.specific) return(-1)
      return(0) # "Weak", "Void", "Zero", "Inexistent"
    }

    if (factor.evaluation == "I") {
      if (resource.evaluation == "Excelent") return(1 + 3/nrfactors)
      if (resource.evaluation == "Good") return(1 + 2/nrfactors)
      if (resource.evaluation == "Regular") return(1 + 1/nrfactors)
      if (resource.evaluation == "Weak") return(1)
      if (resource.evaluation == "Void") return(0.01)
      if (resource.evaluation == "Zero") return(0.001)
      return(0) # "Inexistent"
    }

    stop("fail when agregating  - invalid factor or resource evaluation")
  }



