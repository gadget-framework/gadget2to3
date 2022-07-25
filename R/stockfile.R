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
        stock_var = stock_var), g2_stock[[1]]))

    # Check growthandeatlengths matches stock definition
    if (length(g2_stock[[1]]$growthandeatlengths) > 0) {
        gelengths <- Rgadget::read.gadget.file(path, g2_stock[[1]]$growthandeatlengths)[[1]]
        if (!all.equal(
                seq(g2_stock[[1]]$minlength, g2_stock[[1]]$maxlength - g2_stock[[1]]$dl, g2_stock[[1]]$dl),
                gelengths$lower) || tail(gelengths$upper, 1) != g2_stock[[1]]$maxlength) {
            stop("Gadget3 doesn't support differing growth/eat lengths")
        }
    }

    actions_code <- lapply(seq_along(g2_stock), function (i) {
        sect <- g2_stock[[i]]
        sect_name <- names(g2_stock)[[i]]
        if (i == 1) {
            # First section doesn't have an action, but we should at least have ageing
            substitute(g3a_age(stock_var), list(stock_var = stock_var))
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

stockfile_naturalmortality <- function (path, stock_var, sect, g2_stock) {
    nm <- sect$naturalmortality
    if (which(!duplicated(nm)) == 1) {
        # All the same, can just use the first
        nm_f <- g2to3_formula(nm[[1]])
    } else {
        stop("Not sure how to handle naturalmortality = ", paste(nm))
    }
    substitute(g3a_naturalmortality(stock_var, g3a_naturalmortality_exp(nm_f)), list(
        nm_f = nm_f,
        stock_var = stock_var))
}
