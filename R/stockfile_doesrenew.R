stockfile_doesrenew <- function (path, stock_var, sect, g2_stock) {
    if (sect$dl != g2_stock[[1]]$dl) stop("doesrenew dl should match stock's dl")
    if (sect$minlength != g2_stock[[1]]$minlength) stop("doesrenew minlength should match stock's minlength")
    if (sect$maxlength != g2_stock[[1]]$maxlength) stop("doesrenew maxlength should match stock's maxlength")

    if ('normalparamfile' %in% names(sect)) {
        stockfile_doesrenew_normalparam(path, stock_var, sect, g2_stock)
    } else {
        stop('Unknown initial conditions type ', sect)
    }
}

stockfile_doesrenew_normalparam <- function (path, stock_var, sect, g2_stock) {
    npf <- Rgadget::read.gadget.file(path, sect$normalparamfile, 'data', recursive = FALSE)[[1]]
    if (!identical(names(npf), c('year', 'step', 'area', 'age', 'number', 'mean', 'stddev', 'alpha', 'beta'))) {
        stop("Unknown columns in ", sect$normalparamfile, ": ", paste(names(npf), collapse = ", "))
    }

    run_f <- TRUE

    if (list.all.equal(npf[,'step'])) {
        run_f <- substitute(run_f && cur_step == x, list(
            x = as.integer(npf[1, 'step']),
            run_f = run_f))
    } else {
        run_f <- call("stop", "Can't translate multi-step renewals")
    }

    if (list.all.equal(npf[,'area'])) {
        # TODO: area lookup
        run_f <- substitute(run_f && area == x, list(
            x = as.integer(npf[1, 'area']),
            run_f = run_f))
    } else {
        run_f <- call("stop", "Can't translate multi-area renewals")
    }

    if (list.all.equal(npf[,'age'])) {
        run_f <- substitute(run_f && age == x, list(
            x = as.integer(npf[1, 'age']),
            run_f = run_f))
    } else {
        run_f <- call("stop","Can't translate multi-age renewals")
    }

    factor_f <- lapply(npf[,'number'], g2to3_formula)
    if (list.all.equal(factor_f)) {
        factor_f <- factor_f[[1]]
    } else {
        run_f <- call("stop","TODO: If statements and year")
    }

    mean_f <- lapply(npf[,'mean'], g2to3_formula)
    if (list.all.equal(mean_f)) {
        mean_f <- mean_f[[1]]
    } else {
        run_f <- call("stop", paste0("Can't turn mean into formula: ", mean_f))
    }

    stddev_f <- lapply(npf[,'stddev'], g2to3_formula)
    if (list.all.equal(stddev_f)) {
        stddev_f <- stddev_f[[1]]
    } else {
        run_f <- call("stop", paste0("Can't turn stddev into formula: ", stddev_f))
    }

    alpha_f <- lapply(npf[,'alpha'], g2to3_formula)
    if (list.all.equal(alpha_f)) {
        alpha_f <- alpha_f[[1]]
    } else {
        run_f <- call("stop", paste0("Can't turn alpha into formula: ", alpha_f))
    }

    beta_f <- lapply(npf[,'beta'], g2to3_formula)
    if (list.all.equal(beta_f)) {
        beta_f <- beta_f[[1]]
    } else {
        run_f <- call("stop", paste0("Can't turn beta into formula: ", beta_f))
    }

    substitute(g3a_renewal_normalparam(
        stock_var,
        factor_f = factor_f,
        mean_f = mean_f,
        stddev_f = stddev_f,
        alpha_f = alpha_f,
        beta_f = beta_f,
        run_f = quote(run_f)), list(
            stock_var = stock_var,
            factor_f = factor_f,
            mean_f = mean_f,
            stddev_f = stddev_f,
            alpha_f = alpha_f,
            beta_f = beta_f,
            run_f = run_f))
}
