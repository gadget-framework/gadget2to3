\name{fleetfile}
\alias{g2to3_fleetfile}

\title{Convert gadget2 fleet files}
\description{
  Convert gadget2 fleet files to g3 actions
}

\usage{
g2to3_fleetfile(path, file_name)

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
