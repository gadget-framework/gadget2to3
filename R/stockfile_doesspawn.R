stockfile_doesspawn <- function (path, stock_var, sect, ...) {
    spawnfile <- Rgadget::read.gadget.file(path, sect$spawnfile, recursive = FALSE)[[1]]

    out <- call("g3a_spawn", stock_var)
    out[['recruitment_f']] <- as.call(c(
        as.symbol(paste0("g3a_spawn_recruitment_", spawnfile$recruitment[[1]])),
        lapply(
            tail(spawnfile$recruitment, -1),
            g2to3_formula)))

    out[['proportion_f']] <- stockfile_doesspawn_lengthsel(spawnfile$proportionfunction)
    out[['mortality_f']] <- stockfile_doesspawn_lengthsel(spawnfile$mortalityfunction)
    out[['weightloss_f']] <- stockfile_doesspawn_lengthsel(spawnfile$weightlossfunction)

    output_ratios <- as.numeric(spawnfile$spawnstocksandratios[seq(2, length(spawnfile$spawnstocksandratios), 2)])
    names(output_ratios) <- spawnfile$spawnstocksandratios[seq(1, length(spawnfile$spawnstocksandratios), 2)]
    out[['output_stocks']] <- as.call(c(as.symbol("list"), lapply(names(output_ratios), as.symbol)))
    if(!all(output_ratios == rep(1/length(output_ratios), times = length(output_ratios)))) {
        out[['output_ratios']] <- output_ratios
    }

    out[['mean_f']] <- g2to3_formula(spawnfile$stockparameters[[1]])
    out[['stddev_f']] <- g2to3_formula(spawnfile$stockparameters[[2]])
    out[['alpha_f']] <- g2to3_formula(spawnfile$stockparameters[[3]])
    out[['beta_f']] <- g2to3_formula(spawnfile$stockparameters[[4]])

    run_f <- quote(TRUE)
    run_subs <- list()
    run_f <- substitute(run_f && (x), list(
        x = Reduce(
                function (a, b) substitute(a || cur_step == b, list(a = a, b = as.integer(b))),
                spawnfile$spawnsteps,
                FALSE),
        run_f = run_f))
    run_f <- substitute(run_f && (x), list(
        x = Reduce(function (a, b) {
            # We don't hae area_names available here, so turn run_f into a substitute
            # call that will put the right value in the gap.
            temp_var_name <- paste0('area_names_', b)
            run_subs[[temp_var_name]] <<- substitute(area_names[b], list(b = as.character(b)))
            substitute(a || area == temp_var_name, list(
                temp_var_name = as.symbol(temp_var_name),
                a = a))
        }, spawnfile$spawnareas, FALSE),
        run_f = run_f))
    if ('firstspawnyear' %in% names(spawnfile)) run_f <- substitute(run_f && (cur_year >= x), list(
        x = as.integer(spawnfile$firstspawnyear)))
    if ('lastspawnyear' %in% names(spawnfile)) run_f <- substitute(run_f && (cur_year <= x), list(
        x = as.integer(spawnfile$lastspawnyear)))

    if (length(run_subs) > 0) {
        out[['run_f']] <- call("substitute",
            gadget3:::f_optimize(run_f),
            as.call(c( as.symbol("list"), run_subs )))
    } else {
        out[['run_f']] <- call("quote", gadget3:::f_optimize(run_f))
    }

    return(out)
}

stockfile_doesspawn_lengthsel <- function(sel) {
    if (sel[[1]] == 'exponential') {
        call("g3_suitability_exponentiall50",
            # NB: exponentiall50 needs to negate alpha to match gadget2
            alpha = gadget3:::f_substitute(quote(-x), list(x = g2to3_formula(sel[[2]]))),
            l50 = g2to3_formula(sel[[3]]))
    } else if (sel[[1]] == 'straightline') {
        call("g3_suitability_straightline",
            alpha = g2to3_formula(sel[[2]]),
            beta = g2to3_formula(sel[[3]]))
    } else if (sel[[1]] == 'constant') {
        g2to3_formula(sel[[2]])
    } else {
        substitute(stop("Unknown selectivity function ", sel), list(sel = sel))
    }
}
