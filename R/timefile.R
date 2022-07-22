g2to3_timefile <- function (path, file_name) {
    g2_time <- Rgadget::read.gadget.file(path, file_name, 'time', recursive = FALSE)
    if (length(g2_time[[1]]$firststep) == 1 && g2_time[[1]]$firststep != 1) stop("firststep must be 1 (initial part-years not supported)")
    
    substitute(
        actions_time <- list(g3a_time(
            firstyear,
            lastyear,
            step_lengths = step_lengths,
            final_year_steps = final_year_steps)), list(
        firstyear = g2_time[[1]]$firstyear,
        lastyear = g2_time[[1]]$lastyear,
        step_lengths = tail(g2_time[[1]]$notimesteps, -1),
        final_year_steps = g2_time[[1]]$laststep))
}
