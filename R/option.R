
# TODO(Taranti) method para coletar os fatores utilizados.
# TODO(Taranti) method para instanciar com data.frame e list(specific factors) nome
# TODO(Taranti) criar lista de Degree of importance para ser usada na validacao
#  do criterion




#' Option S4 Class
#'
#' Option S4 class.  ... TODO(Pessoa) VRF eAmpliar
#'
#' @slot name character
#' @slot option.resources Option.resources
#'
#' @export
#' @include option-resources.R
setClass(
  "Option",
  representation(
    name = "character",
    option.resources = "Option.resources"),
  validity = function(object) {
    if (!methods::is(object@option.resources, "Option.resources")) stop("@option.resources must be a Option.resources S4 object")

    if (length(object@name) > 1) stop("@name cannot have more then 1 value")
    if (object@name == "") stop("@name cannot be void")
    if (grepl("^\\s*$", object@name)) stop("@name cannot be only blanc spaces")
    TRUE
  }
)



setMethod(
  f = "initialize",
  signature = "Option",
  definition = function(.Object,
                        name,
                        option.resources){
    cat(" ~~~ \n")
    # Assignment of the slots
    .Object@name <- name
    .Object@option.resources = option.resources
    methods::validObject(.Object)
    return(.Object)
    # return of the object
  }
)

#' Option Constructor function
#'
#'
#' Constructs a Option S4 object.  ... TODO(Pessoa) VRF e Ampliar
#'
#' @param  name charactere
#' @param  option.resources Option.criteria S4 object
#'
#' @return a \code{\link{Option}} S4 object
#'
#' @export
#'
#' @examples
#' \dontrun{Option <- Option(name, option.resources)}
#'
#'
#'
Option <- function(name, option.resources){
  new("Option", name, option.resources)
}




#' Title
#'
#' @param option
#'
#' @return
#' @export
#'
#' @examples
getOptionFactorsNames <- function(option){
  list.of.factors.names <- list()
  for (option.factor.availability in option@option.resources@list.of.factor.availability) {
    list.of.factors.names <- list(list.of.factors.names, option.factor.availability@factor@name )

  }
  vector.of.factors.names <- unlist(list.of.factors.names)
  vector.of.factors.names <- sort(vector.of.factors.names, decreasing = FALSE)
  return(vector.of.factors.names)
}


