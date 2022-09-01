g2to3_likelihoodfile <- function (path, file_name) {
    g2_likelihoods <- Rgadget::read.gadget.file(path, file_name, 'likelihood', recursive = FALSE)
    
    as.call(c(as.symbol("{"),  # }
        lapply(g2_likelihoods, function (x) likelihood(path, x, g2_likelihoods))))
}

likelihood <- function (path, g2_likelihood, g2_likelihoods) {
    # Dispatch to real function
    action <- get_g2tog3("likelihood_", g2_likelihood$type)(path, g2_likelihood, g2_likelihoods)
    substitute({
        actions_l_var <- list(action)
    }, list(
        actions_l_var = as.symbol(paste0("actions_lik_", g2_likelihood$name)),
        action = action))
}

likelihood_penalty <- function (path, g2_likelihood, g2_likelihoods) {
    # Penalty isn't useful in gadget3
    NULL
}

likelihood_understocking <- function (path, g2_likelihood, g2_likelihoods) {
    # Guess understocking stocks from all other likelihood components.
    stock_list <- unique(unlist(lapply(g2_likelihoods, function (l) l$stocknames)))
    # Turn back into list call
    stock_list <- as.call(lapply(c("list", stock_list), as.symbol))
    
    substitute(
        g3l_understocking(
            weight = as.double(weight),
            nll_breakdown = TRUE,
            stock_list), list(
        weight = g2_likelihood$weight,
        stock_list = stock_list))
}

likelihood_catchdistribution <- function (path, g2_likelihood, g2_likelihoods) {
    if (g2_likelihood[['function']] == 'sumofsquares') {
        function_f <- call("g3l_distribution_sumofsquares")
    } else if (g2_likelihood[['function']] == 'multinomial') {
        function_f <- call("g3l_distribution_multinomial", epsilon = as.double(g2_likelihood$epsilon))
    } else {
        stop("Unknown distribution type", )
    }
    if (length(g2_likelihood$overconsumption) == 1 && g2_likelihood$overconsumption > 0) {
        if (any(vapply(g2_likelihoods, function (l) l$type == "understocking", logical(1)))) {
            warning("Likelhood overconsumption=1, as well as a separate understocking component. This should be unnecessary, assuming overconsumption=0")
        } else {
            stop("Likelihood overconsumption=1 not supported")
        }
    }
    if (length(g2_likelihood$aggregationlevel) == 1 && g2_likelihood$aggregationlevel > 0) stop("Likelihood aggregationlevel=1 not supported")
    likelihood_common(path, g2_likelihood, function_f)
}

# Difference with stockdistribution is in the data, which we handle automatically
likelihood_stockdistribution <- likelihood_catchdistribution

likelihood_surveyindices <- function (path, g2_likelihood, ...) {
    if (g2_likelihood$fittype == 'linearfit') {
        function_f <- call("g3l_distribution_surveyindices_linear")
    } else if (g2_likelihood$fittype == 'loglinearfit') {
        function_f <- call("g3l_distribution_surveyindices_log")
    } else if (g2_likelihood$fittype == 'fixedslopelinearfit') {
        function_f <- call("g3l_distribution_surveyindices_linear",
            beta = g2_likelihood$slope)
    } else if (g2_likelihood$fittype == 'fixedslopeloglinearfit') {
        function_f <- call("g3l_distribution_surveyindices_log",
            beta = g2_likelihood$slope)
    } else if (g2_likelihood$fittype == 'fixedinterceptlinearfit') {
        function_f <- call("g3l_distribution_surveyindices_linear",
            alpha = g2_likelihood$intercept)
    } else if (g2_likelihood$fittype == 'fixedinterceptloglinearfit') {
        function_f <- call("g3l_distribution_surveyindices_log",
            alpha = g2_likelihood$intercept)
    } else if (g2_likelihood$fittype == 'fixedlinearfit') {
        function_f <- call("g3l_distribution_surveyindices_linear",
            alpha = g2_likelihood$intercept,
            beta = g2_likelihood$slope)
    } else if (g2_likelihood$fittype == 'fixedloglinearfit') {
        function_f <- call("g3l_distribution_surveyindices_log",
            alpha = g2_likelihood$intercept,
            beta = g2_likelihood$slope)
    } else {
        function_f <- call("stop", paste0("Unknown surveyindices fit type ", g2_likelihood$fittype))
    }
    likelihood_common(path, g2_likelihood, function_f)
}

likelihood_catchinkilos <- function (path, g2_likelihood, ...) {
    if (g2_likelihood[['function']] == 'sumofsquares') {
        function_f <- call("g3l_distribution_sumofsquaredlogratios")
    } else {
        function_f <- call("stop", paste0("Unknown surveyindices fit type ", g2_likelihood[['function']]))
    }
    likelihood_common(path, g2_likelihood, function_f)
}

likelihood_common <- function (path, g2_likelihood, function_f) {
    to_list_call <- function (strings) {
        as.call(lapply(c("list", strings), as.symbol))
    }

    # Generate call to parse data files
    obs_data_call <- call('g2to3_aggdata', path, g2_likelihood$datafile)
    for (agg in c('areaaggfile', 'ageaggfile', 'lenaggfile')) {
        if (agg %in% names(g2_likelihood)) obs_data_call[agg] <- g2_likelihood[[agg]]
    }

    if (identical(g2_likelihood$biomass, 1)) {
        # surveyindices is grouping by weight
        obs_data_call[['final_colname']] <- 'weight'
    } else {
        obs_data_call[['final_colname']] <- 'number'
    }

    method_name <- if (length(g2_likelihood$fleetnames) > 0) "g3l_catchdistribution" else "g3l_abundancedistribution"
    out <- call(method_name, g2_likelihood$name, obs_data_call)
    if (length(g2_likelihood$fleetnames) > 0) out['fleets'] <- list(to_list_call(g2_likelihood$fleetnames))
    out['stocks'] <- list(to_list_call(g2_likelihood$stocknames))
    out['function_f'] <- list(function_f)
    out['area_group'] <- list(quote( area_names ))
    out['report'] <- TRUE
    out['nll_breakdown'] <- TRUE
    out['weight'] <- as.double(g2_likelihood$weight)
    return(out)
}
