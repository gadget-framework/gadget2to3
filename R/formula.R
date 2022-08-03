g2to3_formula <- function (path, g2f) {
    if (is.character(g2f) && file.exists(file.path(path, g2f))) {
        # TODO: Read Time/stock variable
        return(substitute(stop(str), list(str = paste0("Cannot parse Time/Stock variable ", g2f))))
    }

    out <- if (is.call(g2f)) g2f else if (is.na(g2f)) quote(NA) else Rgadget::parse.gadget.formulae(g2f)
    subs <- lapply(all.vars(out), function (x) substitute(g3_param(x), list(x=x)))
    names(subs) <- all.vars(out)
    out <- gadget3:::f_substitute(out, subs)

    # It's a numeric constant, no point being a formula
    if (length(out) == 2 && is.numeric(out[[2]])) out <- out[[2]]

    return(out)
}
