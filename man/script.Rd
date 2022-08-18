\name{script}
\alias{g2to3_script}

\title{Convert gadget2 model}
\description{
  Convert gadget2 model into standalone g3 script
}

\usage{
g2to3_script(
        path,
        output_path,
        output_script = "run.R",
        mainfile_name = "main",
        paramfile_name = "params.in",
        r_model = TRUE,
        tmb_model = TRUE)

}

\arguments{
  \item{path}{
    Root of a gadget2 model directory.
  }
  \item{output_path}{
    Directory to write script, and ancillary data, to.
  }
  \item{output_script}{
    Script containing g3 model.
  }
  \item{mainfile_name}{
    File name of gadget2 mainfile, relative to \var{path}.
  }
  \item{paramfile_name}{
    File name of gadget2 params.in, relative to \var{path}.
  }
  \item{r_model}{Include code to generate an R model?}
  \item{tmb_model}{Include code to generate a TMB model?}
}

\details{
  As well as generating code as-per \code{\link{g2to3_mainfile}}, this will
  convert any data-extracting calls (e.g. \code{\link{g2to3_aggdata}}), extract
  the relevant data and save it instead as an .RDS file.
}

\value{
  The \var{output_path} used.
}

\examples{
# Generate a standalone g3 script
\dontrun{
g2to3_script('06-ling', 'g3-ling')
source('g3-ling/run.R', chdir = T, echo = T)
}
}
