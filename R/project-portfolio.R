# criar metodo para instanciar a partir de data.frame
#




#' Project.portfoio S4 Class
#'
#' Project.portfoio S4 class contains a list of S4 Project objects.
#' # TODO (Pessoa) explicar que é input com o resources.portfolio e a matrix de
#' agregacao.
#'
#' @slot list.of.project list of Project S4 objects
#'
#' @export
#'
#' @include project.R
#'
setClass(
  "Project.portfolio",
  representation(
    list.of.project = "list"),

  validity = function(object) {

    # not null
    if (is.null(object@list.of.project))
      stop("@list.of.project cannot be NULL")
    #is.data.frame(df) && nrow(df)==0

    # is list and have elements
    if (!(is.list(object@list.of.project) &&
          length(object@list.of.project) > 0))
      stop("list.of.project must be a list with one or more
           Project")

    # all elements are Project
    for (project in object@list.of.project) {
      if (!methods::is(project, "Project"))
        stop("@list.of.project must be a list of
             Project S4 objects")
    }

    project.names <- c()
    for (project in object@list.of.project) {
      project.names <- c(project.names, project@name)}
    #print(project.names)
    if (anyDuplicated(project.names) > 0) stop("Project names must be different
                                               from each other. Check -> ",
                                               project.names )
  }
)



setMethod(
  f = "initialize",
  signature = "Project.portfolio",
  definition = function(.Object,
                        list.of.project){
    cat("~~~ Project.portfolio: initializator ~~~ \n")
    # Assignment of the slots
    .Object@list.of.project <- list.of.project
    methods::validObject(.Object)
    return(.Object)
    # return of the object
  }
)

#' Project.portfolio Constructor
#'
#' Project.portfolio(list.of.project) is a constructor to Project.portfolio S4 objects.
#'
#' @param list.of.project list of Project S4 objects
#'
#' @return a \code{\link{Project.portfolio}} S4 object
#'
#' @export
#'
#' @examples
#' \dontrun{Project.portfolio(list(project1, project2, project3))}
#' # TODO(Taranti) inserir exemplo
#'
Project.portfolio <- function(list.of.project){
  new("Project.portfolio", list.of.project)
}






