g2to3_formula <- function (path, g2f) {
    if (is.character(g2f) && file.exists(file.path(path, g2f))) {
        tvfile <- Rgadget::read.gadget.file(path, g2f, file_type = 'timevariable', recursive = FALSE)

        if (is.null(tvfile[[1]]$timedata)) {
          return(substitute( stop("No timedata in timevariable file", g2f), g2f = g2f))
        }

        fs <- lapply(tvfile[[1]]$timedata$value, function (x) g2to3_formula(path, x))
        names(fs) <- paste0(tvfile[[1]]$timedata$year, '-', tvfile[[1]]$timedata$step)
        names(fs)[[1]] <- 'init'

        return(gadget3::g3_timevariable(names(tvfile[[1]])[[1]], fs))
    }

    out <- if (is.call(g2f)) g2f else if (is.na(g2f)) quote(NA) else Rgadget::parse.gadget.formulae(g2f)
    subs <- lapply(all.vars(out), function (x) substitute(g3_param(x), list(x=x)))
    names(subs) <- all.vars(out)
    out <- gadget3:::f_substitute(out, subs)

    # It's a numeric constant, no point being a formula
    if (length(out) == 2 && is.numeric(out[[2]])) out <- out[[2]]

    return(out)
}
