# criar metodo para instanciar a partir de data.frame
#




#' Project.portfolio S4 Class
#'
#' Project.portfolio S4 class contains a list of S4 Project objects.
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


#' Title
#'
#' @param project.portfolio.as.data.frame
#' @param project.portfolio.specifics.as.data.frame
#'
#' @return
#' @export
#'
#' @examples
Project.portfolio2 <- function(project.portfolio.as.data.frame,
                               project.portfolio.specifics.as.data.frame){

  project.names <- row.names(project.portfolio.as.data.frame)
  factors.names <- colnames(project.portfolio.as.data.frame)
  Project.portfolio(
    lapply( i <- 1:length(project.names), function(i) {
      Project(
        project.names[[i]],
        Project.criteria(
          lapply( x <- 1:length(factors.names), function(x) {
            Project.criterion(
              Factor(factors.names[[x]]),
              project.portfolio.as.data.frame[i,x],
              project.portfolio.specifics.as.data.frame[i,x]
            )
          }
          )
        )
      )
    }
    )
  )
}



#' Title
#'
#' @param project.portfolio
#'
#' @return
#' @export
#'
#' @examples
getProjectPortfolioAsDataFrame <- function(project.portfolio){

  portfolio.factors <- getProjectPortfolioFactors(project.portfolio)
  project.portfolio.names <- getProjectPortfolioNames(project.portfolio)

  df <- data.frame(matrix(ncol = length(portfolio.factors), nrow = length(project.portfolio.names)))
  colnames(df) <- portfolio.factors
  rownames(df) <- project.portfolio.names
  for (project in project.portfolio@list.of.project) {
    for (project.criterion in project@project.criteria@list.of.project.criterion) {
      df[project@name, project.criterion@factor@name] <- project.criterion@importance.degree
      #print(project.criterion@factor@name, project, project.criterion@importance.degree)
      }
  }
  return(df)
}





#' Title
#'
#' @param project.portfolio
#'
#' @return
#' @export
#'
#' @examples
getProjectPortfolioSpecificsAsDataFrame <- function(project.portfolio){

portfolio.factors <- getProjectPortfolioFactors(project.portfolio)
project.portfolio.names <- getProjectPortfolioNames(project.portfolio)

df <- data.frame(matrix(ncol = length(portfolio.factors), nrow = length(project.portfolio.names)))
colnames(df) <- portfolio.factors
rownames(df) <- project.portfolio.names
for (project in project.portfolio@list.of.project) {
  for (project.criterion in project@project.criteria@list.of.project.criterion) {
    df[project@name, project.criterion@factor@name] <- project.criterion@specific
    #print(project.criterion@factor@name, project, project.criterion@importance.degree)
  }
}
# change any "" or "   "  to NA
#data.frame <- as.data.frame(apply(data.frame,2,function(x)gsub("^\\s*$", NA,x)))

return(df)
}



#' Title
#'
#' @param project.portfolio
#'
#' @return
#' @export
#'
#' @examples
getProjectPortfolioFactors <- function(project.portfolio){
  vector.of.factors <- NULL
  for (project in project.portfolio@list.of.project) {
    vector.of.factors <- c(vector.of.factors, getProjectFactorsNames(project))
  }
  vector.of.factors <- sort(vector.of.factors, decreasing = FALSE)
  vector.of.factors <- unique(vector.of.factors)
  return(vector.of.factors)
}




#' Title
#'
#' @param project.portfolio
#'
#' @return
#' @export
#'
#' @examples
getProjectPortfolioNames <- function(project.portfolio){
vector.of.names <- NULL
for (project in project.portfolio@list.of.project) {
  vector.of.names <- c(vector.of.names, project@name)
}
vector.of.names <- sort(vector.of.names, decreasing = FALSE)
return(vector.of.names)
}
