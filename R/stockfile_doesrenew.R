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
        area_var_name <- paste0('area_', as.character(npf[1, 'area']))
        run_f <- substitute(run_f && area == area_var_name, list(
            area_var_name = as.symbol(area_var_name),
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

    out <- call('g3a_renewal_normalparam', stock_var)
    out[['factor_f']] <- combine_formulas(
        lapply(npf[,'number'], function(x) g2to3_formula(path, x)),
        cur_year = as.integer(npf[, 'year']))

    out[['mean_f']] <- combine_formulas(
        lapply(npf[,'mean'], function(x) g2to3_formula(path, x)),
        cur_year = as.integer(npf[, 'year']))

    out[['stddev_f']] <- combine_formulas(
        lapply(npf[,'stddev'], function(x) g2to3_formula(path, x)),
        cur_year = as.integer(npf[, 'year']))

    out[['alpha_f']] <- combine_formulas(
        lapply(npf[,'alpha'], function(x) g2to3_formula(path, x)),
        cur_year = as.integer(npf[, 'year']))

    out[['beta_f']] <- combine_formulas(
        lapply(npf[,'beta'], function(x) g2to3_formula(path, x)),
        cur_year = as.integer(npf[, 'year']))

    out[['run_f']] <- call("quote", gadget3:::f_optimize(run_f))

    return(out)
}
