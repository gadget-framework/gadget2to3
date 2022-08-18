\name{mainfile}
\alias{g2to3_mainfile}

\title{Convert gadget2 mainfile}
\description{
  Convert gadget2 main file and all constituent files within
}

\usage{
g2to3_mainfile(path, file_name = 'main')

}

\arguments{
  \item{path}{
    Root of a gadget2 model directory.
  }
  \item{file_name}{
    File name of gadget2 file to convert, relative to \var{path}.
  }
}

\value{
  Unevaluated code to generate gadget3 stocks & actions.
}

\examples{
# Generate a gadget3 R model from a gadget2 model
\dontrun{
actions <- eval(g2to3_mainfile('06-ling'))
model_fn <- g3_to_r(actions)
}
}
