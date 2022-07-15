stockfile_initialconditions <- function (path, stock_var, sect, g2_stock) {
    if (sect$dl != g2_stock[[1]]$dl) stop("Initialconditions dl should match stock's dl")
    if (sect$minlength != g2_stock[[1]]$minlength) stop("Initialconditions dl should match stock's minlength")
    if (sect$maxlength != g2_stock[[1]]$maxlength) stop("Initialconditions dl should match stock's maxlength")
    if (sect$minage != g2_stock[[1]]$minage) stop("Initialconditions dl should match stock's minage")
    if (sect$maxage != g2_stock[[1]]$maxage) stop("Initialconditions dl should match stock's maxage")

    npf <- Rgadget::read.gadget.file(path, sect$normalparamfile, 'data', recursive = FALSE)[[1]]

    age_factor_f <- lapply(seq_len(nrow(npf)), function (i) {
        modify_fns <- list(function (num) as.symbol("age"))
        names(modify_fns) <- npf[i, 'age']
        call_replace(g2to3_formula(npf[i, 'age.factor'][[1]]), modify_fns)
    })
    if (list.all.equal(age_factor_f)) {
        age_factor_f <- age_factor_f[[1]]
    } else {
        stop("Can't turn age.factor into formula: ", age_factor_f)
    }

    area_factor_f <- lapply(seq_len(nrow(npf)), function (i) {
        modify_fns <- list(function (num) as.symbol("area"))
        names(modify_fns) <- npf[i, 'area']
        call_replace(g2to3_formula(npf[i, 'area.factor'][[1]]), modify_fns)
    })
    if (list.all.equal(area_factor_f)) {
        area_factor_f <- area_factor_f[[1]]
    } else {
        stop("Can't turn area.factor into formula: ", area_factor_f)
    }

    mean_f <- lapply(npf[,'mean'], g2to3_formula)
    if (list.all.equal(mean_f)) {
        mean_f <- mean_f[[1]]
    } else {
        stop("Can't turn mean into formula: ", mean_f)
    }

    stddev_f <- lapply(npf[,'stddev'], g2to3_formula)
    if (list.all.equal(stddev_f)) {
        stddev_f <- stddev_f[[1]]
    } else {
        stop("Can't turn stddev into formula: ", stddev_f)
    }

    alpha_f <- lapply(npf[,'alpha'], g2to3_formula)
    if (list.all.equal(alpha_f)) {
        alpha_f <- alpha_f[[1]]
    } else {
        stop("Can't turn alpha into formula: ", alpha_f)
    }

    beta_f <- lapply(npf[,'beta'], g2to3_formula)
    if (list.all.equal(beta_f)) {
        beta_f <- beta_f[[1]]
    } else {
        stop("Can't turn beta into formula: ", beta_f)
    }

    substitute(g3a_initialconditions_normalparam(
        stock_var,
        factor_f = factor_f,
        mean_f = mean_f,
        stddev_f = stddev_f,
        alpha_f = alpha_f,
        beta_f = beta_f), list(
            stock_var = stock_var,
            factor_f = gadget3:::f_substitute(quote( age_factor_f * area_factor_f ),
                list(age_factor_f = age_factor_f, area_factor_f = area_factor_f)),
            mean_f = mean_f,
            stddev_f = stddev_f,
            alpha_f = alpha_f,
            beta_f = beta_f))
}
