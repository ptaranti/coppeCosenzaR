% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/project-portfolio.R
\docType{methods}
\name{Project.portfolio}
\alias{Project.portfolio}
\alias{Project.portfolio,ANY,ANY-method}
\alias{Project.portfolio,list,ANY-method}
\alias{Project.portfolio,data.frame,data.frame-method}
\title{Project.portfolio}
\usage{
Project.portfolio(x, y)

\S4method{Project.portfolio}{ANY,ANY}(x)

\S4method{Project.portfolio}{list,ANY}(x)

\S4method{Project.portfolio}{data.frame,data.frame}(x, y)
}
\arguments{
\item{x}{list A non-empty list with Project S4 objects, or a data frame with
factors evauation}

\item{y}{data.frame with specfic factors, if x is also a data.frame}
}
\value{
a Project.portfolio S4 object
}
\description{
S4 method to construct Project.portfolio S4 objects. It accepts different
sets for parameters types.
}
\note{
Arguments (ANY) \cr
 A call to \code{Project.portfolio( )} with no parameters will return
 an error message for missing argument.

Arguments (data.frame, data.frame). Data.frame where columns represent
factors and rows are the projects. The data.frame is checked for no-columns
and no-rows. The firs data.frame contain the factors evaluation and the
second, with same rows and columns, contain boolean information about the
factor being specific or not to the project.
The constructors called subsequently will verify if acceptable values where
used to factor evaluation and for distinct names of factors and projects

It is possible to obtain a dummy table to serve as example by
construction a portfolio using  \code{Project.portfolio(list.of.projects)}
and, after, converting it in a data.frame using the function
\code{as.data.frame(project.portfolio)}.
}
\examples{
\dontrun{option.portfolio <- Project.portfolio(list.of.project)}

\dontrun{project.portfolio <-
(project.portfolio.as.data.frame, project.portfolio.specifics.as.data.frame)}


}
