\name{stockfile}
\alias{g2to3_stockfile}

\title{Convert gadget2 stockfiles}
\description{
  Convert gadget2 stockfiles to g3 actions
}

\usage{
g2to3_stockfile(path, file_name)

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
