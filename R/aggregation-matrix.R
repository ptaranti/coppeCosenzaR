#' Aggregation.matrix S4 Class
#'
#' This class was included to act as an abstract class to be inherited by
#' concrete classes that implement their matrix in constructors.
#'
#'
#' @export
#'
setClass(
  "Aggregation.matrix",
  representation( ),
    validity = function(object) {
      TRUE
      }

  )



setMethod(
  f = "initialize",
  signature = "Aggregation.matrix",
    definition = function(.Object)
                          {
      # cat("~~~ Aggregation.matrix: initializator ~~~ \n")
      # Assignment of the slots
      methods::validObject(.Object)
      return(.Object)
      # return of the object
    }
  )






#' AggregateMatrix
#'
#' S4 method to perform Aggregation.Matrix inheirited objects. The method to be
#' executed is selected by the Ag
#' # TODO(Pessoa) explicar aqui
#'
#' @export
#'
#' @return data.frame
#'
#'
setGeneric(
  "AggregateMatrix",
  function(
    aggregation.matrix,
    project.portfolio.as.data.frame,
    project.portfolio.specifics.as.data.frame,
    option.portfolio.as.data.frame)
    standardGeneric("AggregateMatrix"),
  signature = c("aggregation.matrix",
    "project.portfolio.as.data.frame",
    "project.portfolio.specifics.as.data.frame",
    "option.portfolio.as.data.frame")
  )

