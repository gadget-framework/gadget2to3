stockfile_doesgrow <- function (path, stock_var, sect, g2_stock) {
    func_name <- tolower(sect$growthfunction)
    if (func_name == 'lengthvbsimple') {
        delta_len_f <- substitute(g3a_grow_lengthvbsimple(linf, kappa), list(
            linf  = g2to3_formula(path, sect$growthparameters[[1]]),
            kappa = g2to3_formula(path, sect$growthparameters[[2]])))
        delta_wgt_f <- substitute(g3a_grow_weightsimple(alpha, beta), list(
            alpha = g2to3_formula(path, sect$growthparameters[[3]]),
            beta  = g2to3_formula(path, sect$growthparameters[[4]])))
    } else {
        delta_len_f <- substitute( stop("Can't translate ", fn, " with params: ", params), list(
            fn = sect$growthfunction,
            params = paste0(sect$growthparameters, collapse = ", ")))
        delta_wgt_f <- substitute( stop("Can't translate ", fn, " with params: ", params), list(
            fn = sect$growthfunction,
            params = paste0(sect$growthparameters, collapse = ", ")))
    }

    out <- call('g3a_growmature', stock_var, impl_f = substitute(
        g3a_grow_impl_bbinom(
            delta_len_f, delta_wgt_f,
            beta_f = beta_f,
            maxlengthgroupgrowth = mlgg), list(
                delta_len_f = delta_len_f,
                delta_wgt_f = delta_wgt_f,
                beta_f = g2to3_formula(path, sect$beta[[1]]),
                mlgg = as.integer(sect$maxlengthgroupgrowth[[1]]))))

    if (g2_stock[['doesmature']]$doesmature == 1) {
        sect <- g2_stock[['doesmature']]
        matfile <- Rgadget::read.gadget.file(path, sect$maturityfile, recursive = FALSE)[[1]]
        func_name <- tolower(sect$maturityfunction)

        if (func_name == "constant") {
            out[['maturity_f']] <- substitute(g3a_mature_constant(
                alpha = alpha, l50 = l50, beta = beta, a50 = a50), list(
                    alpha = g2to3_formula(path, matfile$coefficients[[1]]),
                    l50 = g2to3_formula(path, matfile$coefficients[[2]]),
                    beta = g2to3_formula(path, matfile$coefficients[[3]]),
                    a50 = g2to3_formula(path, matfile$coefficients[[4]])))
            out[['transition_f']] <- call("quote", gadget3:::f_optimize(Reduce(
                function (a, b) substitute(a || cur_step == b, list(a = a, b = as.integer(b))),
                matfile$maturitysteps,
                FALSE)))
        } else if (func_name == "continuous") {
            out[['maturity_f']] <- substitute(g3a_mature_continuous(
                alpha = alpha, l50 = l50, beta = beta, a50 = a50), list(
                    alpha = g2to3_formula(path, matfile$coefficients[[1]]),
                    l50 = g2to3_formula(path, matfile$coefficients[[2]]),
                    beta = g2to3_formula(path, matfile$coefficients[[3]]),
                    a50 = g2to3_formula(path, matfile$coefficients[[4]])))
            out[['transition_f']] <- TRUE
        } else {
            out[['maturity_f']] <- substitute( stop("Can't translate ", fn, " with params: ", params), list(
                fn = sect$maturityfunction,
                params = paste0(matfile$coefficients, collapse = ", ")))
        }

        output_ratios <- as.numeric(matfile$maturestocksandratios[seq(2, length(matfile$maturestocksandratios), 2)])
        names(output_ratios) <- matfile$maturestocksandratios[seq(1, length(matfile$maturestocksandratios), 2)]
        out[['output_stocks']] <- as.call(c(as.symbol("list"), lapply(names(output_ratios), as.symbol)))
        if(!all(output_ratios == rep(1/length(output_ratios), times = length(output_ratios)))) {
            out[['output_ratios']] <- output_ratios
        }
    }
    return(out)
}
