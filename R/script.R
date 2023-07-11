g2to3_script <- function(
        path,
        output_path,
        output_script = "run.R",
        mainfile_name = "main",
        paramfile_name = "params.in",
        r_model = TRUE,
        tmb_model = TRUE) {
    if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)
    script_con <- file(paste0(output_path, '/', output_script), open = "wt")
    on.exit(close(script_con))

    writeDeparse <- function (code, line_sep = "\n") {
        if (is.call(code) && code[[1]] == "{") {  # }
            for (l in tail(as.list(code), -1)) {
                # Add a spacing line first time around, for following braces don't bother
                writeDeparse(l, line_sep = "")
                cat(line_sep, file = script_con)
            }
        } else {
            writeLines(deparse(code, width.cutoff = 500L), con = script_con)
        }
    }

    action_code <- g2to3_mainfile(path, file_name = mainfile_name)

    # Hunt for g2to3_timeareadata file uses and write to data file
    action_code <- extract_datacalls(action_code, script_con, output_path)
    cat("\n", file = script_con)

    writeDeparse(action_code)

    if (r_model) {
      writeLines(c('', '', 'model_fn <- g3_to_r(actions)'), con = script_con)
      writeDeparse(g2to3_params_r(path, paramfile_name))
      writeLines(paste0('result <- model_fn(', paramfile_name, ')'), con = script_con)
    }
    if (tmb_model) {
      writeLines(c('', '', 'model_cpp <- g3_to_tmb(actions)'), con = script_con)
      writeDeparse(g2to3_params_tmb(path, paramfile_name))
      writeLines(paste0('obj <- g3_tmb_adfun(model_cpp, ', paramfile_name, ')'), con = script_con)
    }

    return(output_path)
}
