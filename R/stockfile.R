g2to3_stockfile <- function (path, file_name) {
    g2_stock <- Rgadget::read.gadget.file(path, file_name, 'stock', recursive = FALSE)
    stock_var <- as.symbol(g2_stock[[1]]$stockname)

    init_code <- substitute({
        comment(comment_str)
        stock_var <- g3_stock(stockname, seq(minlength, maxlength - dl, dl))
        stock_var <- g3s_livesonareas(stock_var, livesonareas)
        stock_var <- g3s_age(stock_var, minage, maxage)
    }, c(list(
        comment_str = paste0("Create stock definition for ", g2_stock[[1]]$stockname),
        livesonareas = substitute(area_names[x], list(x = as.character(g2_stock[[1]]$livesonareas))),
        minlength = as.numeric(g2_stock[[1]]$minlength),
        maxlength = as.numeric(g2_stock[[1]]$maxlength),
        dl = as.numeric(g2_stock[[1]]$dl),
        stock_var = stock_var), g2_stock[[1]]))

    # Check growthandeatlengths matches stock definition
    if (length(g2_stock[[1]]$growthandeatlengths) > 0) {
        gelengths <- Rgadget::read.gadget.file(path, g2_stock[[1]]$growthandeatlengths)[[1]]
        if (!is.null(gelengths$lower)) {
            # -- data -- has been added, turning it into a data.frame
            gelengths_seq <- as.vector(c(gelengths$lower, tail(gelengths$upper, 1)))
        } else {
            # List of lists
            gelengths_seq <- as.vector(c(
                # All lower bounds
                vapply(gelengths, function(x) x[[1]], numeric(1)),
                # The final upper bound
                tail(gelengths, 1)[[1]][[2]],
                NULL))
        }
        if (!all.equal(
                seq(g2_stock[[1]]$minlength, g2_stock[[1]]$maxlength, g2_stock[[1]]$dl),
                gelengths_seq)) {
            stop("Gadget3 doesn't support differing growth/eat lengths")
        }
    }

    actions_code <- lapply(seq_along(g2_stock), function (i) {
        sect <- g2_stock[[i]]
        sect_name <- names(g2_stock)[[i]]
        if (i == 1) {
            # First section doesn't have an action
        } else if (sect_name == 'doesmove') {
            # Always have movement (i.e. ageing), even when movement to mature stock is disabled
            stockfile_doesmove(path, stock_var, sect, g2_stock)
        } else if (length(sect) == 1 && length(sect[[1]]) == 1 && sect[[1]][[1]] == 0) {
            # Action is disabled, so nothing to do
        } else {
            # Dispatch to a function corresponding to the component
            get_g2tog3("stockfile_", sect_name)(path, stock_var, sect, g2_stock)
        }
    })
    actions_code <- Filter(Negate(is.null), actions_code)
    actions_code <- as.call(c(as.symbol("list"), actions_code))
    actions_code <- substitute({actions_stock_var <- actions_code}, list(
        actions_stock_var = as.symbol(paste0('actions_', stock_var)),
        actions_code = actions_code))

    # NB: We assume this structure in g2to3_mainfile so we put all the stock definitions first
    return(call("{", init_code, actions_code))
}

stockfile_iseaten <- function (path, stock_var, sect, g2_stock) {
    NULL
}

stockfile_doesmove <- function (path, stock_var, sect, g2_stock) {
    # NB: We always age, even if doesmove is turned off
    out <- call("g3a_age", stock_var)
    if (sect[[1]][[1]] == 1) {
        output_ratios <- as.numeric(sect$transitionstocksandratios[seq(2, length(sect$transitionstocksandratios), 2)])
        names(output_ratios) <- sect$transitionstocksandratios[seq(1, length(sect$transitionstocksandratios), 2)]
        out[['output_stocks']] <- as.call(c(as.symbol("list"), lapply(names(output_ratios), as.symbol)))
        out[['output_ratios']] <- output_ratios
        out[['run_f']] = substitute(quote(cur_step == x), list(x = sect$transitionstep))
    }
    return(out)
}

stockfile_naturalmortality <- function (path, stock_var, sect, g2_stock) {
    nm <- sect$naturalmortality
    if (which(!duplicated(nm)) == 1) {
        # All the same, can just use the first
        nm_f <- g2to3_formula(path, nm[[1]])
    } else {
        stop("Not sure how to handle naturalmortality = ", paste(nm))
    }
    substitute(g3a_naturalmortality(stock_var, g3a_naturalmortality_exp(nm_f)), list(
        nm_f = nm_f,
        stock_var = stock_var))
}
