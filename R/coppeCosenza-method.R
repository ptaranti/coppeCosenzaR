



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
           function(project.portfolio, option.portfolio, factors.under.consideration,...)
             standardGeneric("CoppeCosenzaMethod")
)


setMethod("CoppeCosenzaMethod",
          signature("ANY","ANY","ANY"),
          function(project.portfolio, option.portfolio, factors.under.consideration,...)
            stop("CoppeCosenzaMethod not implemented for provided parameters")
)


setMethod("CoppeCosenzaMethod",
          signature("Project.portfolio", "Option.portfolio", "Factors.under.consideration"),
          function(project.portfolio, option.portfolio, factors.under.consideration) {
            cat("CoppeCosenzaMethod assuming default agregation matrix")

            # subset project.portfolio e option.portfolio para a lista de fatores
            #
            # verificar se o numero esta certo, senao informar que existe discrepancia de fatores
            #
            # executar a matriz de agregacao
            # gerar dataframe
            #
            # executar lista de especificos - retirar os projetos nao atendidos e imprimir lista
            #
            # retornar dataframe final
            #


          }
)
