

#' Option.portfolio S4 Class
#'
#' Option.portfolio S4 class contains a type-checked list of S4 Option objects.
#' This object is an argument to construct the CoppeCosenza S4 objects, which,
#' in turn, represents the method solution.
#'
#' Any S4 Option object can be included in the @list.of.options. This means we
#' can have options with different set of factors. It is possible to export and
#' import Option.portfolio to/from data.frame, allowing to store and edit
#' information externally.
#'
#'
#' @slot list.of.option list of Option S4 objects. The option names are checked
#' and must be distinct.
#'
#' @export
#'
#' @include option.R
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



#' Option.portfolio
#'
#' S4 method to construct Option.portfolio S4 objects. It accepts different
#' sets for parameters types.
#'
#' @return a Option.portfolio S4 object
#' @export
#'
setGeneric("Option.portfolio", function(x, y, ...) standardGeneric("Option.portfolio"))


#' @rdname Option.portfolio
#' @param Arguments (ANY) \cr
#'  A call to \code{Project.portfolio( )} with no parameters will return
#'  an error message for mismatch argument.
#'
setMethod("Option.portfolio",
          signature("ANY"),
          function(x,...)
            stop("Option.portfolio constructor not implemented for provided parameters")
)


#' @rdname Option.portfolio
#'
#' @param Arguments list(). A non-empty list with Option S4 objects.
#'
#' @examples
#' \dontrun{option.portfolio <- Option.portfolio(list.of.options)}
#'
setMethod("Option.portfolio",
          signature("list"),
          function(x){
            list.of.option <- x
            new("Option.portfolio", list.of.option)
          }
)


#' @rdname Option.portfolio
#'
#' @param Arguments data.frame. A data.frame where columns represent factors and
#' rows are the options. The data frame is checked for no columns and no rows.
#' The constructors called subsequently will verify if acceptable values where
#' used to factor evaluation and for distinct names of factors and options.
#'
#' @note It is possible to obtain a dummy table to serve as example by
#' construction a potrfolio using  \code{Option.portfolio(list.of.options)} and
#' after converting it in a data.frame using the function
#' \code{getOptionPortfolioAsDataFrame(option.portfolio)}.
#'
#' @examples
#' \dontrun{option.portfolio <- Option.portfolio(my.option.portfolio.data.frame)}
#'
#' @rdname Option.portfolio
#'
#' @include option-portfolio.R
#'
setMethod("Option.portfolio",
          signature("data.frame"),
          function(x){
            option.portfolio.as.data.frame <- x


            if (!(row.names(option.portfolio.as.data.frame) > 0) )
              stop("there is no options in the portfolio")

            if (!(colnames(option.portfolio.as.data.frame) > 0) )
              stop("there is no factors in the portfolio")

            option.names <- row.names(option.portfolio.as.data.frame)
            factors.names <- colnames(option.portfolio.as.data.frame)
            Option.portfolio(
              lapply( i <- 1:length(option.names), function(i) {
                Option(
                  option.names[[i]],
                  Option.resources(
                    lapply( x <- 1:length(factors.names), function(x) {
                      Option.factor.availability(
                        Factor(option.names[[x]]),
                        option.portfolio.as.data.frame[i,x]
                      )
                    }
                    )
                  )
                )
              }
              )
            )
          }
)





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
