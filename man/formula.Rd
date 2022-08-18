\name{formula}
\alias{g2to3_formula}

\title{Convert gadget2 formulas}
\description{
  Convert gadget2 formula (incl. TimeVariables)
}

\usage{
g2to3_formula(path, g2f)

}

\arguments{
  \item{path}{
    Root of a gadget2 model directory.
  }
  \item{g2f}{
    Gadget2 formula to convert, or file name of gadget2 Time Variable to
    convert, relative to \var{path}.
  }
}

\value{
  Formula matching gadget2 definition, with parameters replaced with \code{\link{g3_param}}, and
  TimeVariables using \code{\link{g3_timevariable}}.
}
