# TODO(Taranti) desacoplar o codigo para permitir facil inserção de novas
# matrizes de agregação
# TODO (Taranti) inserir metodo de construcao que, se nao receber lista de
# fatores, elimine projetos e locais com NA, depois selecione a interceção de
# fatores como conjunto maximo


#' Coppe.cosenza S4 Class
#'
#' Coppe.cosenza S4 class represents the solution of the COPPE-Cosenza method.
#' In order to do so, this S4 class contains the final evaluation of the options
#' regarding the studied projects. It presents a data frame presenting the
#' final evaluation of the options regarding each project.
#' If an option does not satisfies project´s specific factors, the option is
#' discarded (a veto operation), with the value "out".  The result also
#' presents relevant  messages list, describing if some evaluation could not be
#' performed due to entry failures or missing evaluations.
#'
#' @slot result data.frame
#' @slot messages list
#' @export
#'
setClass(
  "Coppe.cosenza",
  representation(
    result = "data.frame",
    messages = "list"),
  validity = function(object) {
    if (!is.data.frame(object@result)) stop("@result must be a data.frame" )
    TRUE

    # TODO(Taranti) consider extra validation - columns and rows non empty and
    # distinct. Non empty values, not NA
    # Unit tests
    }

 )



setMethod(
  f = "initialize",
  signature = "Coppe.cosenza",
  definition = function(.Object,
                        result,
                        messages){
    # cat("~~~ CoppeCosenza: initializator ~~~ \n")
    # Assignment of the slots
    .Object@result <- result
    .Object@messages <- messages
    methods::validObject(.Object)
    return(.Object)
    # return of the object
  }
)



#' Coppe.cosenza
#'
#' S4 method to construc Coppe.cosenza objects. The package also provides a way
#' to verify the consistency of the entry data. There are 3 different matrices
#' which are considered for the evaluation purposes: The project' s required
#' factors; The project's description of specific factors; and the options'
#' available level of factors. All the factors must be evaluated by each project
#'  and by each option. The program deconstruct each evaluation so as to verify:
#'  if all the factors are evaluated for each project; if all the factors are
#'  evaluated for each option, and besides, if all the linguistic variables are
#'  the prescribed ones. Such verification avoids incomplete or incorrect
#'  evaluations presenting the correspondent error messages.
#'
#'
#'
#' @return Coppe.cosenza S4 object
#'
setGeneric("Coppe.cosenza", function(x, y, factors.of.interest,
                                     aggregation.matrix.name, normalize)
  standardGeneric("Coppe.cosenza"))


#' @rdname Coppe.cosenza
#' @param Arguments (ANY) A call to \code{Coppe.cosenza( )} with no parameters
#'  will return an error message for missing argument.
#'  @export
setMethod("Coppe.cosenza",
          signature("ANY"),
          function(x)
            stop("Coppe.cosenza constructor not
                 implemented for provided parameters")
)



#' @rdname  Coppe.cosenza
#' @export
setMethod("Coppe.cosenza",
          signature("Project.portfolio", "Option.portfolio",
                    "Factors.of.interest", "missing", "missing"),
          function(x, y, factors.of.interest,
                   aggregation.matrix.name, normalize) {

            return(
              Coppe.cosenza(x, y, factors.of.interest, "default", FALSE)
            )
          }
          )



#' @rdname Coppe.cosenza
#'
#' @param Arguments \itemize{
#' \item Project.portfolio S4 object
#' \item Option.portfolio S4 object
#' \item Factors.of.interest S4 object
#' \item character - the name of Aggregation.matrix to be used. If not provided
#' the default implementation will be used}
#'
#' @include project-portfolio.R
#' @include option-portfolio.R
#' @include factors-under-consideration.R
#'
#' @export
setMethod("Coppe.cosenza",
          signature("Project.portfolio", "Option.portfolio",
                    "Factors.of.interest", "character", "logical"),
          function(x, y, factors.of.interest,
                   aggregation.matrix.name = "default", normalize = FALSE) {
            # change to semantic expressive names
            project.portfolio <- x
            option.portfolio <- y
            aggregation.matrix.name <-
              paste0("Aggregation.matrix.", aggregation.matrix.name)

            # warning.list store informatio to compose S4 CoppeCosenza@messages
            messages.list <- list()

            # verify if all factors are eveluated for the selected portfolios
            if (!CheckSelectFactors(project.portfolio, option.portfolio,
                                    factors.of.interest)) {
              stop("The selected factors are incompatible with the portfolios")
            }


            # remove projects that were not evaluated for any of
            # considered factors
            project.portfolio.as.data.frame <-
              as.data.frame(project.portfolio)
            project.portfolio.as.data.frame <-
              project.portfolio.as.data.frame[,
                                              getFactorsOfInterestNames
                                              (factors.of.interest),
                                              drop = FALSE]
            temp.df <- project.portfolio.as.data.frame[
              is.na(project.portfolio.as.data.frame), , drop = FALSE]
            if (length(rownames(temp.df)) > 0) {
              messages.list <-
                paste0(
                  messages.list,
                  "The following projects have not evoluation for all considered
                  factors and will be disregarded: \n",
                  rownames(temp.df),
                  collapse = " \n"
                  )
            }

            project.portfolio.as.data.frame <-
              na.omit(project.portfolio.as.data.frame)

            project.portfolio.specifics.as.data.frame <-
              as.data.frame(project.portfolio, optional = TRUE)

            project.portfolio.specifics.as.data.frame <-
              project.portfolio.specifics.as.data.frame[
                row.names(project.portfolio.as.data.frame),
                colnames(project.portfolio.as.data.frame),
                drop = FALSE]


            # remove options that were not evaluated for any of
            # considered factors
            option.portfolio.as.data.frame <- as.data.frame(option.portfolio)
            option.portfolio.as.data.frame <-
              option.portfolio.as.data.frame[
                ,
                getFactorsOfInterestNames(factors.of.interest),
                drop = FALSE]
            temp.df <- option.portfolio.as.data.frame[
              is.na(option.portfolio.as.data.frame),
              ,
              drop = FALSE]
            if (length(rownames(temp.df)) > 0) {
              messages.list <- list(messages.list,
                paste0(
                  "The following options have not evoluation for all considered
                  factors and will be disregarded: \n",
                  rownames(temp.df),
                  collapse = " \n"
                ))
            }
            option.portfolio.as.data.frame <-
              na.omit(option.portfolio.as.data.frame)


            # call the Aggregate function for the correct matrix
            aggregation.matrix <- new(aggregation.matrix.name)
            messages.list <- list(messages.list,
              paste0("CoppeCosenza using", aggregation.matrix.name,
                collapse = " "
              ))

            out <- AggregateMatrix(
              aggregation.matrix,
              project.portfolio.as.data.frame,
                project.portfolio.specifics.as.data.frame,
                option.portfolio.as.data.frame)

            coppe.cosenza <- new("Coppe.cosenza",out, messages.list )

            return(coppe.cosenza)
          }
)


#' @rdname  Coppe.cosenza
#' @export
setMethod("Coppe.cosenza",
          signature("Project", "Option.portfolio",
                    "Factors.of.interest", "character", "missing"),
          function(
            x,
            y,
            factors.of.interest,
            aggregation.matrix.name,
            normalize = FALSE) {

          Coppe.cosenza(Project.portfolio(list(x)),
                        y,
                        factors.of.interest,
                        aggregation.matrix.name,
                        normalize = FALSE)
          }
          )




#' @rdname  Coppe.cosenza
#' @export
setMethod("Coppe.cosenza",
          signature("Project.portfolio", "Option",
                    "Factors.of.interest", "character", "logical"),
          function(
            x,
            y,
            factors.of.interest,
            aggregation.matrix.name = "default",
            normalize = FALSE) {

            Coppe.cosenza(x,
                          Option.portfolio(list(y)),
                          factors.of.interest,
                          aggregation.matrix.name = "default",
                          normalize = FALSE)
          }
)



# Function to verify if all factors in factors.of.interest are included
# in project.portfolio and option.portfolio
CheckSelectFactors <-
  function(project.portfolio, option.portfolio, factors.of.interest) {
    factors.of.interest.names <-
      getFactorsOfInterestNames(factors.of.interest)
    project.portfolio.as.data.frame <- as.data.frame(project.portfolio, FALSE)
    option.portfolio.as.data.frame <- as.data.frame(option.portfolio)

    factors.not.in.project.portfolio <-
      setdiff(
        factors.of.interest.names,
        colnames(project.portfolio.as.data.frame)
      )
    factors.not.in.option.portfolio <-
      setdiff(
        factors.of.interest.names,
        colnames(option.portfolio.as.data.frame))
    flag <- TRUE
    if (length(factors.not.in.project.portfolio) > 0) {
      flag <- FALSE
      cat("\nThe following factors are not considered in project portfolio: ",
          factors.not.in.project.portfolio, "\nThere is no project that complies
          with the factors.of.interest list")
    }
    if (length(factors.not.in.option.portfolio) > 0 ) {
      flag <- FALSE
      cat("\nThe following factors are not considered in option portfolio: ",
          factors.not.in.option.portfolio, "\nThere is no option that complies
          with the factors.of.interest list")
    }
    return(flag)
  }





