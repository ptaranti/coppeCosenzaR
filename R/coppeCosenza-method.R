



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
            project.portfolio.as.data.frame <-
              project.portfolio.as.data.frame[!is.na(project.portfolio.as.data.frame),
                                              ,
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

            option.portfolio.as.data.frame <-
              option.portfolio.as.data.frame[!is.na(option.portfolio.as.data.frame),
                                             ,
                                             drop = FALSE]


            if (agregation.matrix == "default" ) {
              print("CoppeCosenzaMethod assuming default agregation matrix")
              ResolveDefaultAgregationMatrix(project.portfolio.as.data.frame, option.portfolio.as.data.frame)

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


ResolveDefaultAgregationMatrix <- function(project.portfolio.as.data.frame, option.portfolio.as.data.frame) {
  print(project.portfolio.as.data.frame)
  print(option.portfolio.as.data.frame)
  print("#TODO implementar a ResolveDefaultAgregationMatrix")

  agregate <- function(x, y, nrfactors){
    if (x == "Cr") {
      if (y == "Excelent") return(1)
      return(0)
    }
    if (x == "C") {
      if (y == "Excelent") return(1 + 1/nrfactors)
      if (y == "Good") return(1)
      return(0)
    }
    if (x == "LC") {
      if (y == "Excelent") return(1 + 2/nrfactors)
      if (y == "Good") return(1 + 1/nrfactors)
      if (y == "Regular") return(1)
      return(0)
    }
    if (x == "I") {
      if (y == "Excelent") return(1 + 3/nrfactors)
      if (y == "Good") return(1 + 2/nrfactors)
      if (y == "Regular") return(1 + 1/nrfactors)
      if (y == "Weak") return(1)
      return(0)
    }
  stop("fail when agregating  - invalid factor or resource evaluation")
  }



}
