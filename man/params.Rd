\name{params}
\alias{g2to3_params_r}
\alias{g2to3_params_tmb}

\title{Convert gadget2 param files into code}
\description{
  Convert gadget2 area files into code to generate g3 params
}

\usage{
g2to3_params_r(path, file_name, model_var_name = 'model_fn', out_var_name = file_name)
g2to3_params_tmb(path, file_name, model_var_name = 'model_cpp', out_var_name = file_name)

}

\arguments{
  \item{path}{
    Root of a gadget2 model directory.
  }
  \item{file_name}{
    File name of gadget2 file to convert, relative to \var{path}.
  }
  \item{model_var_name}{
    Name of variable containing the TMB / R model,
    to extract the parameter template from.
  }
  \item{out_var_name}{
    Variable name to save new parameters into.
  }
}

\value{
  Unevaluated code to extract parameter template from model,
  and customise with all values within parameter file.
}
