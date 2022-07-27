g2to3_timefile <- function (path, file_name) {
    g2_time <- Rgadget::read.gadget.file(path, file_name, 'time', recursive = FALSE)
    if (length(g2_time[[1]]$firststep) == 1 && g2_time[[1]]$firststep != 1) stop("firststep must be 1 (initial part-years not supported)")
    
    out <- call("g3a_time", g2_time[[1]]$firstyear, g2_time[[1]]$lastyear)
    out[['step_lengths']] <- tail(g2_time[[1]]$notimesteps, -1)

    # Only include laststep if it will differ from the default
    if (g2_time[[1]]$laststep != length(out[['step_lengths']])) out[['final_year_steps']] <- g2_time[[1]]$laststep

    substitute(actions_time <- list(out), list(out=out))
}
