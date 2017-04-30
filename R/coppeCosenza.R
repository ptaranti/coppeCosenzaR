# TODO(Taranti) desacoplar o codigo para permitir facil inserção de novas
# matrizes de agregação
# TODO (Taranti) inserir metodo de construcao que, se nao receber lista de
# fatores, elimine projetos e locais com NA, depois selecione a interceção de
# fatores como conjunto maximo


#' Coppe.cosenza S4 Class
#'
#' Coppe.cosenza S4 class represents the solution of the COPPE-Cosenza method.
#' # TODO(Pessoa)Ampliar aqui.
#'
#' @slot result data.frame
#' @slot messages list
#'
#' @export
#'
setClass(
  "Coppe.cosenza",
  representation(
    result = "data.frame",
    messages = "list"),
  validity = function(object) {
    if (!is.data.frame(df)) stop("@result must be a data.frame" )
    TRUE

    # TODO(Taranti) consider extre validation - columns and rows non empty and
    # distinct. Non empty values, not NA
    # Unit tests
    }

 )



setMethod(
  f = "initialize",
  signature = "Coppe.cosenza",
  definition = function(.Object,
                        result,
                        msgs){
    # cat("~~~ CoppeCosenza: initializator ~~~ \n")
    # Assignment of the slots
    .Object@result <- result
    .Object@msgs <- msgs
    methods::validObject(.Object)
    return(.Object)
    # return of the object
  }
)



#' Coppe.cosenza
#'
#' S4 method to construc Coppe.cosenza objects. It solves de
#' # TODO(Pessoa) explicar aqui
#'
#' @export
#'
#' @return Coppe.cosenza S4 object
#'
setGeneric("Coppe.cosenza", function(x, y, z, k,...)
  standardGeneric("Coppe.cosenza"))


#' @rdname Coppe.cosenza
#' @param Arguments (ANY) A call to \code{Coppe.cosenza( )} with no parameters
#'  will return an error message for missing argument.
#'
setMethod("Coppe.cosenza",
          signature("ANY"),
          function(x,...)
            stop("Coppe.cosenza constructor not
                 implemented for provided parameters")
)


#' @rdname Coppe.cosenza
#'
#' @param Arguments \itemize{
#' \item x  a Project.portfolio S4 object
#' \item Option.portfolio S4 object
#' \item Factors.under.consideration S4 object}
#'
#' @include project-portfolio.R
#' @include option-portfolio.R
#' @include factors-under-consideration.R
#'
setMethod("Coppe.cosenza",
          signature("Project.portfolio", "Option.portfolio",
                    "Factors.under.consideration"),
          function(x, y, z) {

            project.portfolio <- x
            option.portfolio <- y
            factors.under.consideration <- z
            agregation.matrix <- "default" # TODO(Taranti) marretato

            if (!CheckSelectFactors(project.portfolio, option.portfolio,
                                    factors.under.consideration)) {
              stop("The selected factors are incompatible with the portfolios")
            }
            project.portfolio.as.data.frame <-
              as.data.frame(project.portfolio)
            project.portfolio.as.data.frame <-
              project.portfolio.as.data.frame[,
                                              getFactorsUnderConsiderationNames
                                              (factors.under.consideration),
                                              drop = FALSE]
            temp.df <- project.portfolio.as.data.frame[
              is.na(project.portfolio.as.data.frame), , drop = FALSE]
            if (length(rownames(temp.df)) > 0) {
              warning("The following projects have not evoluation for all
                      considered factors and will be disregarded: ",
                      rownames(temp.df))
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

            option.portfolio.as.data.frame <- as.data.frame(option.portfolio)
            option.portfolio.as.data.frame <-
              option.portfolio.as.data.frame[
                ,
                getFactorsUnderConsiderationNames(factors.under.consideration),
                drop = FALSE]
            temp.df <- option.portfolio.as.data.frame[
              is.na(option.portfolio.as.data.frame),
              ,
              drop = FALSE]
            if (length(rownames(temp.df)) > 0) {
              warning("The following options have not evoluation for all
                      considered resources and will be disregarded: ",
                      rownames(temp.df))
            }

            option.portfolio.as.data.frame <-
              na.omit(option.portfolio.as.data.frame)


            if (agregation.matrix == "default" ) {
              print("CoppeCosenzaMethod assuming default agregation matrix")
              out <- ResolveDefaultAgregationMatrix(
                project.portfolio.as.data.frame,
                project.portfolio.specifics.as.data.frame,
                option.portfolio.as.data.frame)
            }
            else stop(agregation.matrix,  " not implemented")

            messages <- list() #TODO (Taranti)

            coppe.cosenza <- new("Coppe.cosenza",out, messages )

            return(coppe.cosenza) #TODO (Taranti) não esta funcionando
          }
)


# Function to verify if all factors in factors.under.consideration are included
# in project.portfolio and option.portfolio
CheckSelectFactors <-
  function(project.portfolio, option.portfolio, factors.under.consideration) {
    factors.under.consideration.names <-
      getFactorsUnderConsiderationNames(factors.under.consideration)
    project.portfolio.as.data.frame <- as.data.frame(project.portfolio, FALSE)
    option.portfolio.as.data.frame <- as.data.frame(option.portfolio)

    factors.not.in.project.portfolio <-
      setdiff(
        factors.under.consideration.names,
        colnames(project.portfolio.as.data.frame)
      )
    factors.not.in.option.portfolio <-
      setdiff(
        factors.under.consideration.names,
        colnames(option.portfolio.as.data.frame))
    flag <- TRUE
    if (length(factors.not.in.project.portfolio) > 0) {
      flag <- FALSE
      cat("\nThe following factors are not considered in project portfolio: ",
          factors.not.in.project.portfolio)
    }
    if (length(factors.not.in.option.portfolio) > 0 ) {
      flag <- FALSE
      cat("\nThe following factors are not considered in option portfolio: ",
          factors.not.in.option.portfolio)
    }
    return(flag)
  }




# Function to handle the solving of the agregate matrix. It call agregate for
# Project(j,i) and option(k,i), i vary from 1:number.of.factors
# This function needs a well behavored set of parameters, said: no NA, all
# values already checked and valid, and all project and options evaluated.
ResolveDefaultAgregationMatrix <- function(
  project.portfolio.as.data.frame, project.portfolio.specifics.as.data.frame,
  option.portfolio.as.data.frame) {

  agregation.matrix.temp <-
    data.frame(
      matrix(
        ncol = length(row.names(option.portfolio.as.data.frame)),
        nrow = length(row.names(project.portfolio.as.data.frame))
      )
    )

  colnames(agregation.matrix.temp) <- row.names(option.portfolio.as.data.frame)
  rownames(agregation.matrix.temp) <- row.names(project.portfolio.as.data.frame)

  nrfactors <- length(colnames(project.portfolio.as.data.frame))
  for (i in 1:length(row.names(project.portfolio.as.data.frame))) {
    for (j in 1:length(row.names(option.portfolio.as.data.frame))) {
      temp.list.agregation <- lapply(1:nrfactors, function(x)
        (Agregate(project.portfolio.as.data.frame[i, x],
                  option.portfolio.as.data.frame[j, x],
                  project.portfolio.specifics.as.data.frame[i, x],
                  nrfactors)))
      agregation <- NULL
      if (any(temp.list.agregation == -1)) agregation <- "out"
      else agregation <- sum(unlist(temp.list.agregation))
      agregation.matrix.temp[i,j] <- agregation
    }
  }

  return(agregation.matrix.temp)
}






#' Agregate
#'
#' This function do not validate entries, since it is not exported and the data
#' is validated by the constructors. The validation here would be resource
#' consuming.
#'
#' @param factor.evaluation character factor evaluation from project
#' @param resource.evaluation character factor evaluation from option
#' @param factor.is.specific logic indicates that this factor is specific for
#' the project
#' @param nrfactors numeric number of factors evaluated for each project/option
#'
#' @return numeric indicate the result factor per option. If a specific factor
#' is not achived it returns -1
#'
#'
Agregate <-
  function(
    factor.evaluation,
    resource.evaluation,
    factor.is.specific,
    nrfactors){
    # factor.evaluation  - character
    # resource.evaluation - character
    # factor.is.specific - logical
    # nrfactors - numeric
    if (factor.evaluation == "Cr") {
      if (resource.evaluation == "Excelent") return(1)
      if (factor.is.specific) return(-1)
      return(0) # if -> "Good", "Regular", "Weak", "Void", "Zero", "Inexistent
    }

    if (factor.evaluation == "C") {
      if (resource.evaluation == "Excelent") return(1 + 1/nrfactors)
      if (resource.evaluation == "Good") return(1)
      if (factor.is.specific) return(-1)
      return(0) # "Regular", "Weak", "Void", "Zero", "Inexistent"
    }

    if (factor.evaluation == "LC") {
      if (resource.evaluation == "Excelent") return(1 + 2/nrfactors)
      if (resource.evaluation == "Good") return(1 + 1/nrfactors)
      if (resource.evaluation == "Regular") return(1)
      if (factor.is.specific) return(-1)
      return(0) # "Weak", "Void", "Zero", "Inexistent"
    }

    if (factor.evaluation == "I") {
      if (resource.evaluation == "Excelent") return(1 + 3/nrfactors)
      if (resource.evaluation == "Good") return(1 + 2/nrfactors)
      if (resource.evaluation == "Regular") return(1 + 1/nrfactors)
      if (resource.evaluation == "Weak") return(1)
      if (resource.evaluation == "Void") return(0.01)
      if (resource.evaluation == "Zero") return(0.001)
      return(0) # "Inexistent"
    }

    stop("fail when agregating  - invalid factor or resource evaluation")
  }




