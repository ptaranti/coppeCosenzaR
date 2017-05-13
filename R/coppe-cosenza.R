


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
#' @include  aggregation-matrix.R
#' @export
#'
setClass(
  "Coppe.cosenza",
  representation(
    result = "data.frame",
    messages = "character",
    projects.names = "character",
    options.names = "character",
    aggregation.matrix = "Aggregation.matrix"),
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
                        messages,
                        projects.names,
                        options.names,
                        aggregation.matrix){
    # cat("~~~ CoppeCosenza: initializator ~~~ \n")
    # Assignment of the slots
    .Object@result <- result
    .Object@messages <- messages
    .Object@projects.names <- projects.names
    .Object@options.names <- options.names
    .Object@aggregation.matrix <- aggregation.matrix
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
                                     aggregation.matrix.name = "default",
                                     normalize = FALSE)
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
              Coppe.cosenza(x, y, factors.of.interest,
                            aggregation.matrix.name, normalize)
            )
          }
)

#' @rdname  Coppe.cosenza
#' @export
setMethod("Coppe.cosenza",
          signature("Project.portfolio", "Option.portfolio",
                    "Factors.of.interest", "character", "missing"),
          function(x, y, factors.of.interest,
                   aggregation.matrix.name, normalize) {

            return(
              Coppe.cosenza(x, y, factors.of.interest,
                            aggregation.matrix.name, normalize)
            )
          }
)

#' @rdname  Coppe.cosenza
#' @export
setMethod("Coppe.cosenza",
          signature("Project.portfolio", "Option.portfolio",
                    "Factors.of.interest", "missing", "logical"),
          function(x, y, factors.of.interest,
                   aggregation.matrix.name, normalize) {

            return(
              Coppe.cosenza(x, y, factors.of.interest,
                            aggregation.matrix.name, normalize)
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
#' @include factors-of-interest.R
#'
#' @export
setMethod("Coppe.cosenza",
          signature("Project.portfolio", "Option.portfolio",
                    "Factors.of.interest", "character", "logical"),
          function(x, y, factors.of.interest,
                   aggregation.matrix.name, normalize) {
            # change to semantic expressive names
            project.portfolio <- x
            option.portfolio <- y
            aggregation.matrix.name <-
              paste0("Aggregation.matrix.", aggregation.matrix.name)

            # warning.list store informatio to compose S4 CoppeCosenza@messages
            messages.vector <- c("Warning messages:")

            # verify if all factors are eveluated for the selected portfolios
            if (!CheckSelectFactors(project.portfolio, option.portfolio,
                                    factors.of.interest)) {
              stop("The selected factors are incompatible with the portfolios")
            }

            # remove factors out of interest

            project.portfolio.as.data.frame <-
              as.data.frame(project.portfolio)[,
                                               getFactorsOfInterestNames
                                               (factors.of.interest),
                                               drop = FALSE]

            # remove projects with NA values for any factor

            rows.initial <- row.names(project.portfolio.as.data.frame)

            project.portfolio.as.data.frame <-
              na.omit(project.portfolio.as.data.frame)

            rows.without.NA <- row.names(project.portfolio.as.data.frame)

            rows.whith.NA <-  base::setdiff(rows.initial, rows.without.NA)

            if (length(rows.whith.NA) > 0) {
              messages.vector <- c(messages.vector,
                                    paste0(
                                      "Disregarding projects with NA value for any factors:",
                                      sep = " ", as.character(rows.whith.NA)))
            }

            #setting project specifics data.frame
            project.portfolio.specifics.as.data.frame <-
              as.data.frame(project.portfolio, optional = TRUE)

            project.portfolio.specifics.as.data.frame <-
              project.portfolio.specifics.as.data.frame[
                row.names(project.portfolio.as.data.frame),
                colnames(project.portfolio.as.data.frame),
                drop = FALSE]


            # remove factors out of interest
            option.portfolio.as.data.frame <-
              as.data.frame(option.portfolio)[ ,
                                               getFactorsOfInterestNames
                                               (factors.of.interest),
                                               drop = FALSE]


            # remove options with NA values for any factor

            rows.initial <- row.names(option.portfolio.as.data.frame)

            option.portfolio.as.data.frame <-
              na.omit(option.portfolio.as.data.frame)

            rows.without.NA <- row.names(option.portfolio.as.data.frame)

            rows.whith.NA <-  base::setdiff(rows.initial, rows.without.NA)

            if (length(rows.whith.NA) > 0) {
              messages.vector <- c(messages.vector,
                                    paste0(
                                      "Disregarding options with NA value for any factors:",
                                      sep = " ", as.character(rows.whith.NA)))
            }

            # call the Aggregate function for the correct matrix
            aggregation.matrix <- new(aggregation.matrix.name)


            out <- AggregateMatrix(
              aggregation.matrix,
              project.portfolio.as.data.frame,
              project.portfolio.specifics.as.data.frame,
              option.portfolio.as.data.frame)


            if (normalize == TRUE) {
              messages.vector <- c(messages.vector,"CoppeCosenza normalizing result (using 1/nrfactors)")
              out <- out/length(factors.of.interest@list.of.factors)


            }

            # store col and row names
            names <- colnames(out)
            rows  <- row.names(out)

            # resetting negatives to (-1)
            out <- apply(out, c(1,2), function(x) if (x < 0) return(-1) else return(x))

            #change -1 to "out" -- specific factor not available
            out <- as.data.frame(apply(out,2,function(x)gsub("-1", "out",as.character(x))))
            #resetting col and row names
            names(out) <- names
            rownames(out) <- rows

            coppe.cosenza <- new("Coppe.cosenza",
                                 out,
                                 messages.vector,
                                 getProjectPortfolioNames(project.portfolio),
                                 getOptionPortfolioNames(option.portfolio),
                                 aggregation.matrix )

            return(coppe.cosenza)
          }
)


#' @rdname  Coppe.cosenza
#' @export
setMethod("Coppe.cosenza",
          signature("Project"),
          function(
            x,
            y,
            factors.of.interest,
            aggregation.matrix.name,
            normalize) {

            if (methods::is(y, "Option")) y <- Option.portfolio(list(y))

            Coppe.cosenza(Project.portfolio(list(x)),
                          y,
                          factors.of.interest,
                          aggregation.matrix.name,
                          normalize)
          }
)




#' @rdname  Coppe.cosenza
#' @export
setMethod("Coppe.cosenza",
          signature("Project.portfolio", "Option"),
          function(
            x,
            y,
            factors.of.interest,
            aggregation.matrix.name,
            normalize) {

            Coppe.cosenza(x,
                          Option.portfolio(list(y)),
                          factors.of.interest,
                          aggregation.matrix.name,
                          normalize)
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


#' summary
#'
#' Generic S4 method to \code{\link{summary}}.
#' @export
#'
#setGeneric("summary", function(x, ...) standardGeneric("summary"))
setGeneric("summary", function(object, ...)
  standardGeneric("summary"),
  useAsDefault = base::summary
)

#' @export
setMethod("summary", signature("Coppe.cosenza"),
          function(object) {
            project.list <- row.names(object@result)
            option.list  <- colnames(object@result)

            if (length(project.list) == 1) {
              print("projetos")
            }

            if (length(option.list) == 1) {
              print("opcoes")
            }

            if (length(project.list) != 1 &&
                length(option.list) != 1) {
              df1 <- NULL
              df <- NULL
              for (i in 1:length(project.list) ) {
                for (j in 1:length(option.list) ) {
                  if (as.character(object@result[[i,j]]) != "out")
                    df <- rbind(
                      df,
                      data.frame(project.list[[i]],
                                 option.list[[j]],
                                 object@result[[i,j]]
                      )
                    )
                  else df1 <- rbind(
                    df1,
                    data.frame(project.list[[i]],
                               option.list[[j]]
                    )
                  )
                }
              }

              if (length(row.names(df)) == 0) return("No solution found")

              names(df) <-  c("Project","Option", "Aggregate.value")
              df$Aggregate.value <- as.numeric(as.character(df$Aggregate.value))
              df <- df[order( df[,3] , decreasing = TRUE),]
              cat("\n\n-----------------Coppe.cosenza method---------------------------\n\n")

              if (length(object@messages) > 1) {
                for (txt in object@messages) cat(paste0(txt, sep = " ", "\n"))
              }
              cat("\n----------------\n")
              cat(paste0("Solutions using the ", sep = " ", object@aggregation.matrix@name))
              cat("\n----------------\n")
              print(df)

              names(df1) <-  c("Dropped Project","Incompatible Option")
              df <- df[order( df[,1]),]
              cat(paste0("\n-------------------\nIncompatible solutions using the", sep = " ", object@aggregation.matrix@name, ":"))
              cat("\n--------------------\n")
              print(df1)
              cat("\n--------------------\n")
            }
          }
)

