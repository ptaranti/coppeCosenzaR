
#' Project.criterion S4 Class
#'
#' Project.criterion S4 class. It defines the criterium to be used in
#' association to a factor when evaluatig projects ... TODO(Pessoa) VRF eAmpliar
#'
#' The accepted degrees are: "Cr", "C", "LC","I"
#'
#' @slot factor Factor S4 class
#' @slot importance.degree charactere, must mach the scale of degrees to be used
#' @slot specific logical indicates the considered factors is specific for the
#' project under consideration
#'
#' @export
#' @include factor.R
setClass(
  "Project.criterion",
  representation(
    factor = "Factor",
    importance.degree = "character",
    specific = "logical"),
  validity = function(object) {

    if (!methods::is(object@factor, "Factor"))
      stop("'@factor must be a Factor S4 object")

    accepted.importance.degree <- c("Cr", "C", "LC","I")
    if (!(object@importance.degree %in% accepted.importance.degree))
      stop("'@importance.degree must match an expectded value. Currently:
           c(\"Cr\", \"C\", \"LC\",\"I\")")

    if (!is.logical(object@specific)) stop("@specific must be logical")
    TRUE
  }
)



setMethod(
  f = "initialize",
  signature = "Project.criterion",
  definition = function(.Object,
                        factor,
                        importance.degree,
                        specific){
    # cat("~~~ Project.criterion ~~~ \n")
    # Assignment of the slots
    .Object@factor <- factor
    .Object@importance.degree = importance.degree
    .Object@specific = specific

    methods::validObject(.Object)
    return(.Object)
    # return of the object
  }
)




#' Project.criterion
#'
#' This function is a constructor to Project.criterion S4 class.
#' It defines the criteron to be used in association to a factor when evaluatig
#'  projects ...
#'  TODO(Pessoa) VRF e Ampliar
#'
#' @param  factor Factor S4 class
#' @param  importance.degree character, must mach one item of the scale of
#' degrees to be used ("Cr", "C", "LC","I")
#' @param  specific logical indicates the considered factors is specific for the
#' project under consideration#'
#'
#' @return a \code{\link{Project.criterion}} S4 object
#'
#' @export
#'
#' @examples
#' \dontrun{Project.criterion <- Project.criterion(factor, importance.degree, specific)}
#' Project.criterion(Factor("fator1"), "LC", FALSE)
#'
Project.criterion <- function(factor, importance.degree, specific){
  new("Project.criterion", factor,  as.character(importance.degree), specific)
}

#' @export
setMethod("show", "Project.criterion",
          function(object){
            df <- data.frame(as.character(object@factor@name), object@importance.degree, object@specific)
            names(df) <- c("Factor","importance.degree", "specific")
            print(df)
          }
          )
