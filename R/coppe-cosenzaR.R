


#' @docType package
#' @name coppeCosenzaR
#'
#' @title coppeCosenzaR
#'
#' @description  COPPE-Cosenza Fuzzy Hierarchy Model (coppeCosenzaR).
#'
#' The program implements the COPPE-Cosenza Fuzzy Hierarchy Model .
#'
#' The model was based on the evaluation of local alternatives, representing
#' regional potentialities, so as to fulfill demands of economic projects.
#' After defining demand profiles in terms of their technological coefficients,
#' the degree of importance of factors is defined so as to represent
#' the productive activity.
#'
#' The method can detect a surplus of supply without the restriction of the
#' distance of classical algebra, defining an hierarchy of location
#' alternatives. In Coppe-Cosenza Model, the distance between factors
#' is measured in terms of the difference between grades of  memberships of the
#' same factors belonging to two or more  sets under comparison.
#'
#' The required factors are classified under the following linguistic variables:
#' \itemize{
#' \item Critical (CR),
#' \item Contitioning (C),
#' \item Little Conditioning (LC), and
#' \item Irrelevant (I).
#' }
#'
#' And the alternatives can assume the following linguistic variables:
#' \itemize{
#' \item Excellent (Ex),
#' \item Good (G),
#' \item Regular (R),
#' \item Weak (W),
#' \item Empty (Em),
#' \item Zero (Z), and
#' \item Inexistent (In).
#' }
#'
#' The model also provides flexibility, allowing different aggregation rules to
#' be performed and defined by the Decision Maker. Such feature is considered
#' in this package, allowing the user to define other aggregation matrices,
#' since it considers the same linguistic variables mentioned.
#'
#' The following matrices are avaiable in the package:
#' \itemize{
#' \item Default Matrix (see Aggregation.matrix.default)
#' \item Membership Difference Matrix (see Aggregation.matrix.membership.difference)
#' }#'
#' New matrices can be added when requested.
#'
#' @references
#' Cosenza, Carlos Alberto Nunes, Francisco Antonio Doria, and Leonardo Antonio
#' Monteiro Pessôa. Hierarchy Models for the Organization of Economic Spaces.
#' Procedia Computer Science 55 (2015): 82-91.
#' \url{https://doi.org/10.1016/j.procs.2015.07.010}
#'
#'
#' @author Pier-Giovanni Taranti \email{ptaranti@gmail.com}
#' @author Leonardo Antonio Monteiro Pessoa
#' @author Carlos Alberto Nunes Cosenza
#'
#' @import methods
# @import methods Rgraphviz
# @importClassesFrom graph graphNEL
# @importMethodsFrom graph nodes edgeNames
#' @importFrom stats na.omit
#' @importMethodsFrom methods show
"_PACKAGE"


