g2to3_formula <- function (g2f) {
    out <- if (is.call(g2f)) g2f else if (is.na(g2f)) quote(NA) else Rgadget::parse.gadget.formulae(g2f)
    subs <- lapply(all.vars(out), function (x) substitute(g3_param(x), list(x=x)))
    names(subs) <- all.vars(out)
    gadget3:::f_substitute(out, subs)
}
