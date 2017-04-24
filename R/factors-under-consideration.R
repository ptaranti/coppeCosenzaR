

#' Factors.under.consideration S4 Class
#'
#' Factors.under.consideration S4 class contains a list of S4 Factor objects.
#' This list is   used to construct Projec objects.
#'
#' @slot list.of.factors list of Factor
#'
#' @export
#'
#' @include factor.R
#'
setClass(
  "Factors.under.consideration",
  representation(
    list.of.factors = "list"),
  validity = function(object) {
      msg <- NULL
      if (is.null(object@list.of.factors)) stop("Factors.under.consideration must have one
                                    or more Factors")
      for (factor in object@list.of.factors) {
        if (!methods::is(factor, "Factor"))
          msg <-
            c(msg, "'@all' must be a list of Factor S4 objects")
      }
      if (is.null(msg))
        TRUE
      else
        stop(msg)
    }
)



setMethod(
  f = "initialize",
  signature = "Factors.under.consideration",
  definition = function(.Object,
                        list.of.factors){
    cat("~~~ Factors.under.consideration: initializator ~~~ \n")
    # Assignment of the slots
    .Object@list.of.factors <- list.of.factors
    methods::validObject(.Object)
    return(.Object)
    # return of the object
  }
)

#' Factors.under.consideration(list.of.factors)
#'
#' Factors.under.consideration(list.of.factors) is a constructor.
#'
#' @param list.of.factors list of Factor S4 objects
#'
#' @return a \code{\link{Factors.under.consideration}} S4 object
#'
#' @export
#'
#' @examples
#' Factors.under.consideration(list(Factor("factor1"), Factor("factor2"), Factor("factor3")))
#'
#' @include factor.R
#'
Factors.under.consideration <- function(list.of.factors){
  new("Factors.under.consideration", list.of.factors)
}



#' Title
#'
#' @param factors.under.consideration
#'
#' @return
#' @export
#'
#' @examples
getFactorsUnderConsiderationNames <- function(factors.under.consideration){
  vector.of.factors.names <- NULL
  for (factor in factors.under.consideration@list.of.factors) {
    vector.of.factors.names <- c(vector.of.factors.names, factor@name)
  }
  vector.of.factors.names <- sort(vector.of.factors.names, decreasing = FALSE)
  vector.of.factors.names <- unique(vector.of.factors.names)
  return(vector.of.factors.names)
}



