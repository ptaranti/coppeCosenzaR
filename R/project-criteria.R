
#' Project.criteria S4 Class
#'
#' Project.criteria S4 class contains a list of S4 Project.criterion objects.
#' This list is   used to construct Projec objects.
#'
#' @slot list.of.project.criterion list of Project.criterion
#'
#' @export
#'
#' @include project-criterion.R
#'
setClass(
  "Project.criteria",
  representation(
    list.of.project.criterion = "list"),
  validity = function(object) {

    # not null
    if (is.null(object@list.of.project.criterion))
      stop("@list.of.project.criterion cannot be NULL")
    #is.data.frame(df) && nrow(df)==0

    # is list and have elements
    if (!(is.list(object@list.of.project.criterion) &&
          length(object@list.of.project.criterion) > 0))
      stop("list.of.project.criterion must be a list with one or more
           Project.criterion")

    # all elements are Project.criterion
    for (project.criterion in object@list.of.project.criterion) {
      if (!methods::is(project.criterion, "Project.criterion"))
        stop("@list.of.project.criterion must be a list of
             Project.criterion S4 objects")
    }

    factor.names <- c()
    for (project.criterion in object@list.of.project.criterion) {
         factor.names <- c(factor.names, project.criterion@factor@name)}
    #print(factor.names)
    if (anyDuplicated(factor.names) > 0) stop("Only one criterion for each
                                              factor is allowed. Check -> ",
                                              factor.names )

  }
)



setMethod(
  f = "initialize",
  signature = "Project.criteria",
  definition = function(.Object,
                        list.of.project.criterion){
    cat("~~~ Project.criteria: initializator ~~~ \n")
    # Assignment of the slots
    .Object@list.of.project.criterion <- list.of.project.criterion
    methods::validObject(.Object)
    return(.Object)
    # return of the object
  }
)

#' Project.criteria Constructor
#'
#' Project.criteria(list) is a constructor to Factor S4 objects.
#'
#' @param list list of Project.criterion S4 objects
#'
#' @return a \code{\link{Project.criteria}} S4 object
#'
#' @export
#'
#' @examples
#' # TODO(Taranti) inserir exemplo
#'
Project.criteria <- function(list.of.project.criterion){
  new("Project.criteria", list.of.project.criterion)
}






