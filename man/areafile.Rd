\name{areafile}
\alias{g2to3_areafile}

\title{Convert gadget2 area files}
\description{
  Convert gadget2 area files into g3 actions
}

\usage{
g2to3_areafile(path, file_name)

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
  Unevaluated code to generate an \code{area_names} name lookup.
  Other actions will then expect this variable to be defined.
}
