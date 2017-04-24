
# TODO(Taranti) method para coletar os fatores utilizados.
# TODO(Taranti) method para instanciar com data.frame e list(specific factors) nome
# TODO(Taranti) criar lista de Degree of importance para ser usada na validacao
#  do criterion




#' Project S4 Class
#'
#' Project S4 class.  ... TODO(Pessoa) VRF eAmpliar
#'
#' @slot name character
#' @slot project.criteria Project.criteria
#'
#' @export
#' @include project-criteria.R
setClass(
  "Project",
  representation(
    name = "character",
    project.criteria = "Project.criteria"),
  validity = function(object) {
    if (!methods::is(object@project.criteria, "Project.criteria")) stop("@project.criteria must be a Project.criteria S4 object")

    if (length(object@name) > 1) stop("@name cannot have more then 1 value")
    if (object@name == "") stop("@name cannot be void")
    if (grepl("^\\s*$", object@name)) stop("@name cannot be only blanc spaces")
    TRUE
  }
)



setMethod(
  f = "initialize",
  signature = "Project",
  definition = function(.Object,
                        name,
                        project.criteria){
    cat(" ~~~ \n")
    # Assignment of the slots
    .Object@name <- name
    .Object@project.criteria = project.criteria
    methods::validObject(.Object)
    return(.Object)
    # return of the object
  }
)

#' Project Constructor function
#'
#'
#' Constructs a Project S4 object.  ... TODO(Pessoa) VRF e Ampliar
#'
#' @param  name charactere
#' @param  project.criteria Project.criteria S4 object
#'
#' @return a \code{\link{Project}} S4 object
#'
#' @export
#'
#' @examples
#' \dontrun{Project <- Project(name, project.criteria)}
#'
#'
#'
Project <- function(name, project.criteria){
  new("Project", name, project.criteria)
}


#' Title
#'
#' @param project
#'
#' @return
#' @export
#'
#' @examples
getProjectFactorsNames <- function(project){
  list.of.factors.names <- list()
  for (project.criterion in project@project.criteria@list.of.project.criterion) {
    list.of.factors.names <- list(list.of.factors.names, project.criterion@factor@name )

  }
  vector.of.factors.names <- unlist(list.of.factors.names)
  vector.of.factors.names <- sort(vector.of.factors.names, decreasing = FALSE)
  return(vector.of.factors.names)
}


#' Title
#'
#' @param project
#'
#' @return
#' @export
#'
#' @examples
getProjectFactorsSpecific <- function(project){
  list.of.factors.names <- list()
  for (project.criterion in project@project.criteria@list.of.project.criterion) {
    if (project.criterion@specific == TRUE) {
    list.of.factors.names <- list(list.of.factors.names, project.criterion@factor@name )
    }
  }
  vector.of.factors.names <- unlist(list.of.factors.names)
  vector.of.factors.names <- sort(vector.of.factors.names, decreasing = FALSE)
  return(vector.of.factors.names)
}
