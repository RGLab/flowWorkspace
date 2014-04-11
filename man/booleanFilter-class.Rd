\name{booleanFilter-class}
\alias{booleanFilter}
\alias{booleanFilter-class}
\alias{char2booleanFilter}
\alias{show}
\alias{show,booleanFilter-method}
\title{A class describing logical operation (& or |) of the reference populations}
\usage{
  booleanFilter(expr, ...,
    filterId = "defaultBooleanFilter")

  char2booleanFilter(expr, ...,
    filterId = "defaultBooleanFilter")
}
\arguments{
  \item{expr}{\code{expression} or \code{character}}

  \item{...}{further arguments to the expression}

  \item{filterId}{\code{character} identifier}
}
\description{
  \code{booleanFilter} class inherits class
  \code{\link{expressionFilter}} and exists for the purpose
  of methods dispatching.

  \code{booleanFilter} is a constructor from an expression

  \code{char2booleanFilter} is a constructor from a
  character string
}
\seealso{
  \code{\link{add}} \code{\linkS4class{GatingHierarchy}}
}

