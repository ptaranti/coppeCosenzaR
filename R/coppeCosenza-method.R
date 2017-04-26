



#' CoppeCosenzaMethod
#'
#'
#' @return
#' @export
#'
#' @include project-portfolio.R
#' @include option-portfolio.R
#' @include factors-under-consideration.R
#'
setGeneric("CoppeCosenzaMethod",
           function(project.portfolio, option.portfolio,
                    factors.under.consideration,...)
             standardGeneric("CoppeCosenzaMethod")
)


setMethod("CoppeCosenzaMethod",
          signature("ANY","ANY","ANY"),
          function(project.portfolio, option.portfolio,
                   factors.under.consideration,...)
            stop("CoppeCosenzaMethod not implemented for provided parameters")
)


setMethod("CoppeCosenzaMethod",
          signature("Project.portfolio", "Option.portfolio",
                    "Factors.under.consideration"),
          function(project.portfolio, option.portfolio,
                   factors.under.consideration, agregation.matrix = "default") {
            if (!CheckSelectFactors(project.portfolio, option.portfolio,
                                    factors.under.consideration)) {
              stop("The selected factors are incompatible with the portfolios")
            }
            project.portfolio.as.data.frame <-
              getProjectPortfolioAsDataFrame(project.portfolio)
            project.portfolio.as.data.frame <-
              project.portfolio.as.data.frame[,
                                              getFactorsUnderConsiderationNames(factors.under.consideration),
                                              drop = FALSE]
            temp.df <- project.portfolio.as.data.frame[is.na(project.portfolio.as.data.frame),
                                                       ,
                                                       drop = FALSE]
            if (length(rownames(temp.df)) > 0) {
              warning("The following projects have not evoluation for all
                      considered factors and will be disregarded: ",
                      rownames(temp.df))
            }
            project.portfolio.as.data.frame <- na.omit(project.portfolio.as.data.frame)

            project.portfolio.specifics.as.data.frame <-
              getProjectPortfolioSpecificsAsDataFrame(project.portfolio)

            project.portfolio.specifics.as.data.frame <-
              project.portfolio.specifics.as.data.frame[row.names(project.portfolio.as.data.frame),
                                                        colnames(project.portfolio.as.data.frame),
                                                        drop = FALSE]

            option.portfolio.as.data.frame <- getOptionPortfolioAsDataFrame(option.portfolio)
            option.portfolio.as.data.frame <-
              option.portfolio.as.data.frame[,
                                             getFactorsUnderConsiderationNames(factors.under.consideration),
                                             drop = FALSE]
            temp.df <- option.portfolio.as.data.frame[is.na(option.portfolio.as.data.frame),
                                                      ,
                                                      drop = FALSE]
            if (length(rownames(temp.df)) > 0) {
              warning("The following options have not evoluation for all
                      considered resources and will be disregarded: ",
                      rownames(temp.df))
            }

            option.portfolio.as.data.frame <- na.omit(option.portfolio.as.data.frame)


            if (agregation.matrix == "default" ) {
              print("CoppeCosenzaMethod assuming default agregation matrix")
              ResolveDefaultAgregationMatrix(project.portfolio.as.data.frame,
                                             project.portfolio.specifics.as.data.frame,
                                             option.portfolio.as.data.frame)

            }
            else stop(agregation.matrix,  " not implemented")
          }
)


CheckSelectFactors <- function(project.portfolio, option.portfolio, factors.under.consideration) {
  factors.under.consideration.names <- getFactorsUnderConsiderationNames(factors.under.consideration)
  project.portfolio.as.data.frame <- getProjectPortfolioAsDataFrame(project.portfolio)
  option.portfolio.as.data.frame <- getOptionPortfolioAsDataFrame(option.portfolio)

  factors.not.in.project.portfolio <- setdiff(factors.under.consideration.names, colnames(project.portfolio.as.data.frame))
  factors.not.in.option.portfolio <- setdiff(factors.under.consideration.names, colnames(option.portfolio.as.data.frame))
  flag <- TRUE
  if (length(factors.not.in.project.portfolio) > 0) {
    flag <- FALSE
    cat("\nThe following factors are not considered in project portfolio: ", factors.not.in.project.portfolio)
  }
  if (length(factors.not.in.option.portfolio) > 0 ) {
    flag <- FALSE
    cat("\nThe following factors are not considered in option portfolio: ", factors.not.in.option.portfolio)
  }
  return(flag)
}





ResolveDefaultAgregationMatrix <- function(project.portfolio.as.data.frame,
                                           project.portfolio.specifics.as.data.frame,
                                           option.portfolio.as.data.frame) {
  print("ResolveDefaultAgregationMatrix >>>> \n\n")
  print("project.portfolio.as.data.frame >>>> \n\n")
  print(project.portfolio.as.data.frame)
  print("option.portfolio.as.data.frame >>>> \n\n")
  print(option.portfolio.as.data.frame)

  agregation.matrix.temp <- data.frame(matrix(ncol = length(row.names(option.portfolio.as.data.frame)), nrow = length(row.names(project.portfolio.as.data.frame))))
  colnames(agregation.matrix.temp) <- row.names(option.portfolio.as.data.frame)
  rownames(agregation.matrix.temp) <- row.names(project.portfolio.as.data.frame)
  print("agregation.matrix.temp >>>> \n\n")
  print(agregation.matrix.temp)
  nrfactors <- length(colnames(project.portfolio.as.data.frame))
  for (i in 1:length(row.names(project.portfolio.as.data.frame))) {
    message("\n\nagregating project ", i)
    for (j in 1:length(row.names(option.portfolio.as.data.frame))) {
      message("\nagregating option " , j)
      temp.list.agregation <- lapply(1:nrfactors, function(x)
        (agregate(project.portfolio.as.data.frame[i, x],
                  option.portfolio.as.data.frame[j, x],
                  project.portfolio.specifics.as.data.frame[i, x],
                  nrfactors)))
      agregation <- NULL
      if (any(temp.list.agregation == -1)) agregation <- "out"
      else agregation <- sum(unlist(temp.list.agregation))
      agregation.matrix.temp[i,j] <- agregation
    }
  }
  print(agregation.matrix.temp)
}






#' Title
#'
#' @param factor.evaluation
#' @param resource.evaluation
#' @param factor.is.specific
#' @param nrfactors
#'
#' @return
#' @export
#'
#' @examples
agregate <- function(factor.evaluation, resource.evaluation, factor.is.specific, nrfactors){
  message("agregate")
  message(factor.evaluation, " ", resource.evaluation, " ", factor.is.specific, " ", nrfactors)
  if (factor.evaluation == "Cr") {
    if (resource.evaluation == "Excelent") return(1)
    if (factor.is.specific) return(-1)
    return(0)
  }
  if (factor.evaluation == "C") {
    if (resource.evaluation == "Excelent") return(1 + 1/nrfactors)
    if (resource.evaluation == "Good") return(1)
    if (factor.is.specific) return(-1)
    return(0)
  }
  if (factor.evaluation == "LC") {
    if (resource.evaluation == "Excelent") return(1 + 2/nrfactors)
    if (resource.evaluation == "Good") return(1 + 1/nrfactors)
    if (resource.evaluation == "Regular") return(1)
    if (factor.is.specific) return(-1)
    return(0)
  }
  if (factor.evaluation == "I") {
    if (resource.evaluation == "Excelent") return(1 + 3/nrfactors)
    if (resource.evaluation == "Good") return(1 + 2/nrfactors)
    if (resource.evaluation == "Regular") return(1 + 1/nrfactors)
    if (resource.evaluation == "Weak") return(1)
    return(0)
  }
  stop("fail when agregating  - invalid factor or resource evaluation")
}




