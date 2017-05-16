
#' Option.factor.availability S4 Class
#'
#' Option.factor.availability S4 class. It defines the availability to be used
#' in association to a factor when evaluatig projects ...
#' TODO(Pessoa) VRF eAmpliar
#'
#' The accepted degrees are: Excellent (Ex), Good (G), Regular (R), Weak (W),
#' Empty (Em), Zero (Z), and Inexistent (In).
#'
#' @slot factor Factor S4 class
#' @slot availability character, must mach the scale of degrees to be used
#'
#' @export
#' @include factor.R
setClass(
  "Option.factor.availability",
  representation(
    factor = "Factor",
    availability = "character"),
  validity = function(object) {
    if (!methods::is(object@factor, "Factor"))
      stop("@factor must be a Factor S4 object")
    accepted.availability <-
      c("Ex", "G", "R", "W", "Em", "Z", "In")
    if (!(object@availability %in% accepted.availability))
      stop("'@availability must match an expectded value. Currently:
           c( \"Ex\", \" G \", \" R \", \"W\", \"Em\", \"Z\", \"In\")")
    TRUE
  }
)



setMethod(
  f = "initialize",
  signature = "Option.factor.availability",
  definition = function(.Object,
                        factor,
                        availability){
    #cat("~~~ Option.factor.availability ~~~ \n")
    # Assignment of the slots
    .Object@factor <- factor
    .Object@availability = availability
    methods::validObject(.Object)
    return(.Object)
    # return of the object
  }
)

#' Option.factor.availability Constructor
#'
#' Constructs a Option.factor.availability S4 class. This defines the criterion
#' to be used in association to a factor when evaluatig projects ...
#' TODO(Pessoa) VRF e Ampliar
#'
#' @param  factor Factor S4 class
#' @param  availability character, must mach the scale of degrees as provided in
#' \code{\link{Option.factor.availability}} class documentation
#'
#' @return a \code{\link{Option.factor.availability}} S4 object
#'
#' @export
#'
#' @examples
#' \dontrun{Option.factor.availability <- Option.factor.availability(factor, availability)}
#' Option.factor.availability(Factor("fator1"), "Excellent")
#'
#'
Option.factor.availability <- function(factor, availability){
  new("Option.factor.availability", factor, as.character(availability))
}

#' @export
setMethod("show", "Option.factor.availability",
          function(object){
            df <- data.frame(row.names = as.character(object@factor@name), object@availability)
            names(df) <- c("availability")
            print(df)
          }
)

#' #' @export
#' setMethod("show", "Project.criterion",
#'           function(object){
#'             df <- NULL
#'             df <- data.frame(object@importance.degree, object@specific)
#'             names(df) <- c("importance.degree", "specific")
#'             row.names(df) <- object@factor@name
#'             print(df)
#'           }
#' )


