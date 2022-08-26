stockfile_initialconditions <- function (path, stock_var, sect, g2_stock) {
    if (sect$dl != g2_stock[[1]]$dl) stop("Initialconditions dl should match stock's dl")
    if (sect$minlength != g2_stock[[1]]$minlength) stop("Initialconditions minlength should match stock's minlength")
    if (sect$maxlength != g2_stock[[1]]$maxlength) stop("Initialconditions maxlength should match stock's maxlength")
    if (sect$minage != g2_stock[[1]]$minage) stop("Initialconditions minage should match stock's minage")
    if (sect$maxage != g2_stock[[1]]$maxage) stop("Initialconditions maxage should match stock's maxage")

    if ('normalparamfile' %in% names(sect)) {
        stockfile_initialconditions_normalparam(path, stock_var, sect, g2_stock)
    } else if ('normalcondfile' %in% names(sect) || 'initstockfile' %in% names(sect)) {
        stockfile_initialconditions_normalcond(path, stock_var, sect, g2_stock)
    } else {
        stop('Unknown initial conditions type ', sect)
    }
}

stockfile_initialconditions_normalparam <- function (path, stock_var, sect, g2_stock) {
    npf <- Rgadget::read.gadget.file(path, sect$normalparamfile, 'data', recursive = FALSE)[[1]]
    if (!identical(names(npf), c('age', 'area', 'age.factor', 'area.factor', 'mean', 'stddev', 'alpha', 'beta'))) {
        stop("Unknown columns in ", sect$normalparamfile, ": ", paste(names(npf), collapse = ", "))
    }

    # Generate area symbols to use when generating if statements
    area_syms <- lapply(npf[, 'area'], function(x) as.symbol(paste0('area_', x)))

    age_factor_f <- lapply(seq_len(nrow(npf)), function (i) {
        modify_fns <- list(function (num) as.symbol("age"))
        names(modify_fns) <- npf[i, 'age']
        call_replace(g2to3_formula(path, npf[i, 'age.factor'][[1]]), modify_fns)
    })
    age_factor_f <- combine_formulas(age_factor_f, age = as.integer(npf[, 'age']), area = area_syms)

    area_factor_f <- lapply(seq_len(nrow(npf)), function (i) {
        modify_fns <- list(function (num) as.symbol("area"))
        names(modify_fns) <- npf[i, 'area']
        call_replace(g2to3_formula(path, npf[i, 'area.factor'][[1]]), modify_fns)
    })
    area_factor_f <- combine_formulas(area_factor_f, age = as.integer(npf[, 'age']), area = area_syms)

    out <- call("g3a_initialconditions_normalparam", stock_var)
    out[['factor_f']] <- gadget3:::f_substitute(
        quote( age_factor_f * area_factor_f ),
        list(age_factor_f = age_factor_f, area_factor_f = area_factor_f))

    out[['mean_f']] <- combine_formulas(
        lapply(npf[,'mean'], function(x) g2to3_formula(path, x)),
        age = as.integer(npf[, 'age']),
        area = area_syms)

    out[['stddev_f']] <- combine_formulas(
        lapply(npf[,'stddev'], function(x) g2to3_formula(path, x)),
        age = as.integer(npf[, 'age']),
        area = area_syms)

    out[['alpha_f']] <- combine_formulas(
        lapply(npf[,'alpha'], function(x) g2to3_formula(path, x)),
        age = as.integer(npf[, 'age']),
        area = area_syms)

    out[['beta_f']] <- combine_formulas(
        lapply(npf[,'beta'], function(x) g2to3_formula(path, x)),
        age = as.integer(npf[, 'age']),
        area = area_syms)

    return(out)
}

# normalcond is normalparam but with a reference weight file to use
stockfile_initialconditions_normalcond <- function (path, stock_var, sect, g2_stock) {
    if ('normalcondfile' %in% names(sect)) {
        npf <- Rgadget::read.gadget.file(path, sect$normalcondfile, 'data', recursive = FALSE)[[1]]
    } else {
        npf <- Rgadget::read.gadget.file(path, sect$initstockfile, 'data', recursive = FALSE)[[1]]
    }
    if (!identical(names(npf), c('age', 'area', 'age.factor', 'area.factor', 'mean', 'stddev', 'condition'))) {
        stop("Unknown columns in ", sect$normalparamfile, ": ", paste(names(npf), collapse = ", "))
    }

    # Generate area symbols to use when generating if statements
    area_syms <- lapply(npf[, 'area'], function(x) as.symbol(paste0('area_', x)))

    age_factor_f <- lapply(seq_len(nrow(npf)), function (i) {
        modify_fns <- list(function (num) as.symbol("age"))
        names(modify_fns) <- npf[i, 'age']
        call_replace(g2to3_formula(path, npf[i, 'age.factor'][[1]]), modify_fns)
    })
    age_factor_f <- combine_formulas(age_factor_f, age = as.integer(npf[, 'age']), area = area_syms)

    area_factor_f <- lapply(seq_len(nrow(npf)), function (i) {
        modify_fns <- list(function (num) as.symbol("area"))
        names(modify_fns) <- npf[i, 'area']
        call_replace(g2to3_formula(path, npf[i, 'area.factor'][[1]]), modify_fns)
    })
    area_factor_f <- combine_formulas(area_factor_f, age = as.integer(npf[, 'age']), area = area_syms)

    out <- call("g3a_initialconditions_normalparam", stock_var)
    out[['factor_f']] <- gadget3:::f_substitute(
        quote( age_factor_f * area_factor_f ),
        list(age_factor_f = age_factor_f, area_factor_f = area_factor_f))

    out[['mean_f']] <- combine_formulas(
        lapply(npf[,'mean'], function(x) g2to3_formula(path, x)),
        age = as.integer(npf[, 'age']),
        area = area_syms)

    out[['stddev_f']] <- combine_formulas(
        lapply(npf[,'stddev'], function(x) g2to3_formula(path, x)),
        age = as.integer(npf[, 'age']),
        area = area_syms)

    condition <- combine_formulas(
        lapply(npf[,'condition'], function(x) g2to3_formula(path, x)),
        age = as.integer(npf[, 'age']),
        area = area_syms)
    out[['alpha_f']] <- substitute(
        ~refweight_var * condition,
        list(
            refweight_var = as.symbol(paste0(stock_var, "_refweight")),
            condition = condition))

    out[['beta_f']] <- 0

    return(out)
}
