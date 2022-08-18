\name{likelihoodfile}
\alias{g2to3_likelihoodfile}

\title{Convert gadget2 likelihood files}
\description{
  Convert gadget2 likelihood files into g3 actions
}

\usage{
g2to3_likelihoodfile(path, file_name)

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
  Unevaluated code to generate corresponding gadget3 actions.
}
