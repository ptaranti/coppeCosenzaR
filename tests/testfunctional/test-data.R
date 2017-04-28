

#' Title
#'
#' @return
#' @export
#'
#' @examples
#'
#' @include project-portfolio.R
#' @include option-portfolio.R
#' @include factors-under-consideration.R
#'
FuntionalTest <- function(){

project.portfolio <- Project.portfolio(
  list(
    ###################### Projeto 1
    Project(
      "p1",
      Project.criteria(
        list(
          Project.criterion(
            Factor("fat1"),
            "Cr",
            TRUE
          ),
          Project.criterion(
            Factor("fat2"),
            "C",
            FALSE
          ),
          Project.criterion(
            Factor("fat3"),
            "Cr",
            FALSE
          )
        )
      )
    ),
    ###################### Projeto 2
    Project(
      "p2",
      Project.criteria(
        list(
          Project.criterion(
            Factor("fat1"),
            "Cr",
            TRUE
          ),
          Project.criterion(
            Factor("fat2"),
            "Cr",
            TRUE
          ),
          Project.criterion(
            Factor("fat3"),
            "Cr",
            FALSE
          )
        )
      )
    ),
    ###################### Projeto 3
    Project(
      "p3",
      Project.criteria(
        list(
          Project.criterion(
            Factor("fat1"),
            "C",
            TRUE
          ),
          Project.criterion(
            Factor("fat2"),
            "Cr",
            FALSE
          ),
          Project.criterion(
            Factor("fat3"),
            "Cr",
            FALSE
          )
        )
      )
    ),
    ###################### Projeto 4
    Project(
      "p4",
      Project.criteria(
        list(
          Project.criterion(
            Factor("fat1"),
            "LC",
            FALSE
          ),
          Project.criterion(
            Factor("fat2"),
            "I",
            FALSE
          ),
          Project.criterion(
            Factor("fat3"),
            "LC",
            FALSE
          )
        )
      )
    )
  )
)


option.portfolio <- Option.portfolio(
  list(
    ##############################  Option 1
    Option(
      name = "O1",
      Option.resources(
        list(
          Option.factor.availability(
            Factor("fat1"),
            "Weak"
          ),
          Option.factor.availability(
            Factor("fat2"),
            "Excellent"
          ),
          Option.factor.availability(
            Factor("fat3"),
            "Regular"
          )
        )
      )
    ),
    ##############################  Option 2
    Option(
      name = "O2",
      Option.resources(
        list(
          Option.factor.availability(
            Factor("fat1"),
            "Regular"
          ),
          Option.factor.availability(
            Factor("fat2"),
            "Weak"
          ),
          Option.factor.availability(
            Factor("fat3"),
            "Regular"
          )
        )
      )
    ),
    ##############################  Option 3
    Option(
      name = "O3",
      Option.resources(
        list(
          Option.factor.availability(
            Factor("fat1"),
            "Good"
          ),
          Option.factor.availability(
            Factor("fat2"),
            "Regular"
          ),
          Option.factor.availability(
            Factor("fat3"),
            "Weak"
          )
        )
      )
    )
  )
)


factors.under.consideration <- Factors.under.consideration(
  list(
    Factor("fat1"),
    Factor("fat2"),
    Factor("fat3")
  )
  )

CoppeCosenza(project.portfolio, option.portfolio, factors.under.consideration)

}
