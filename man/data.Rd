\name{data}
\alias{g2to3_data}
\alias{g2to3_timeareadata}
\alias{g2to3_aggdata}

\title{Convert gadget2 data}
\description{
  Convert data components of gadget2 files
}

\usage{
g2to3_data(path, file_name, file_type = 'data', component = 1, sub_component = NULL)
g2to3_timeareadata(path, file_name, areas = NULL)
g2to3_aggdata(
        path,
        file_name,
        areaaggfile = NULL,
        ageaggfile = NULL,
        lenaggfile = NULL,
        final_colname = NULL)

}

\arguments{
  \item{path}{
    Root of a gadget2 model directory.
  }
  \item{file_name}{
    File name of gadget2 file to convert, relative to \var{path}.
  }
  \item{file_type}{
    Rgadget file type to read as.
  }
  \item{component}{
    Component within file to pick out.
  }
  \item{sub_component}{
    Sub-component within component to pick out.
  }
  \item{areas}{
    Reference to area lookup (e.g. \code{area_names}) to hand to \code{link{g3_timeareadata}}.
  }
  \item{areaaggfile}{File name of gadget2 file to convert, relative to \var{path}.}
  \item{ageaggfile}{File name of gadget2 file to convert, relative to \var{path}.}
  \item{lenaggfile}{File name of gadget2 file to convert, relative to \var{path}.}
  \item{final_colname}{
    Rename final column before outputting data.frame (gadget2 doesn't read column definitions, so can't be trusted).
  }
}

\value{
  Unevaluated code to generate corresponding gadget3 actions.
}
