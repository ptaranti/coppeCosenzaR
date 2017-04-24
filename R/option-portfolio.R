# criar metodo para instanciar a partir de data.frame
#




#' Option.portfolio S4 Class
#'
#' Option.portfolio S4 class contains a list of S4 Option objects.
#' # TODO (Pessoa) explicar que é input com o resources.portfolio e a matrix de
#' agregacao.
#'
#' @slot list.of.option list of Option S4 objects
#'
#' @export
#'
#' @include project.R
#'
setClass(
  "Option.portfolio",
  representation(
    list.of.option = "list"),
  validity = function(object) {
    if (is.null(object@list.of.option)) stop("Option.portfolio must have one
                                              or more Option")
    for (project in object@list.of.option) {
      if (!methods::is(project, "Option"))
        stop("@list.of.option must be a list of Option S4 objects")
    }
    project.names <- c()
    for (project in object@list.of.option) {
      project.names <- c(project.names, project@name)}
    #print(project.names)
    if (anyDuplicated(project.names) > 0) stop("project names must be unique
                                               Check -> ", project.names )
  }
)



setMethod(
  f = "initialize",
  signature = "Option.portfolio",
  definition = function(.Object,
                        list.of.option){
    cat("~~~ Option.portfolio: initializator ~~~ \n")
    # Assignment of the slots
    .Object@list.of.option <- list.of.option
    methods::validObject(.Object)
    return(.Object)
    # return of the object
  }
)

#' Option.portfolio Constructor
#'
#' Option.portfolio(list.of.option) is a constructor to Option.portfolio S4 objects.
#'
#' @param list.of.option list of Option S4 objects
#'
#' @return a \code{\link{Option.portfolio}} S4 object
#'
#' @export
#'
#' @examples
#' \dontrun{Option.portfolio(list(project1, project2, project3))}
#' # TODO(Taranti) inserir exemplo
#'
Option.portfolio <- function(list.of.option){
  new("Option.portfolio", list.of.option)
}









#' Title
#'
#' @param option.portfolio
#'
#' @return
#' @export
#'
#' @examples
getOptionPortfolioAsDataFrame <- function(option.portfolio){

  portfolio.factors <- getOptionPortfolioFactors(option.portfolio)
  option.portfolio.names <- getOptionPortfolioNames(option.portfolio)

  df <- data.frame(matrix(ncol = length(portfolio.factors), nrow = length(option.portfolio.names)))
  colnames(df) <- portfolio.factors
  rownames(df) <- option.portfolio.names
  for (option in option.portfolio@list.of.option) {
    for (option.factor.availability in option@option.resources@list.of.factor.availability) {
      df[option@name, option.factor.availability@factor@name] <- option.factor.availability@availability
      #print(project.criterion@factor@name, project, project.criterion@importance.degree)
    }
  }
  return(df)
}









#' Title
#'
#' @param option.portfolio
#'
#' @return
#' @export
#'
#' @examples
getOptionPortfolioFactors <- function(option.portfolio){
  vector.of.factors <- NULL
  for (option in option.portfolio@list.of.option) {
    vector.of.factors <- c(vector.of.factors, getOptionFactorsNames(option))
  }
  vector.of.factors <- sort(vector.of.factors, decreasing = FALSE)
  vector.of.factors <- unique(vector.of.factors)
  return(vector.of.factors)
}





#' Title
#'
#' @param option.portfolio
#'
#' @return
#' @export
#'
#' @examples
getOptionPortfolioNames <- function(option.portfolio){
  vector.of.names <- NULL
  for (option in option.portfolio@list.of.option) {
    vector.of.names <- c(vector.of.names, option@name)
  }
  vector.of.names <- sort(vector.of.names, decreasing = FALSE)
  return(vector.of.names)
}
