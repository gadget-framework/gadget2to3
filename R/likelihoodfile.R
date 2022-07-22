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

likelihood_catchdistribution <- function (path, g2_likelihood, ...) {
    if (g2_likelihood[['function']] == 'sumofsquares') {
        function_f <- call("g3l_distribution_sumofsquares")
    } else if (g2_likelihood[['function']] == 'multinomial') {
        function_f <- call("g3l_distribution_multinomial", epsilon = as.double(g2_likelihood$epsilon))
    } else {
        stop("Unknown distribution type", )
    }
    if (length(g2_likelihood$overconsumption) == 1 && g2_likelihood$overconsumption > 0) stop("Likelihood overconsumption=1 not supported")
    if (length(g2_likelihood$aggregationlevel) == 1 && g2_likelihood$aggregationlevel > 0) stop("Likelihood aggregationlevel=1 not supported")
    likelihood_common(path, g2_likelihood, 'g3l_catchdistribution', function_f)
}

# Difference with stockdistribution is in the data, which we handle automatically
likelihood_stockdistribution <- likelihood_catchdistribution

likelihood_surveyindices <- function (path, g2_likelihood, ...) {
    function_f <- call("g3l_distribution_surveyindices_log")  # TODO: linear or log
    likelihood_common(path, g2_likelihood, 'g3l_abundancedistribution', function_f)
}

likelihood_common <- function (path, g2_likelihood, method_name, function_f) {
    to_list_call <- function (strings) {
        as.call(lapply(c("list", strings), as.symbol))
    }

    # Generate call to parse data files
    obs_data_call <- call('g2to3_aggdata', path, g2_likelihood$datafile)
    for (agg in c('areaaggfile', 'ageaggfile', 'lenaggfile')) {
        if (agg %in% names(g2_likelihood)) obs_data_call[agg] <- g2_likelihood[[agg]]
    }

    out <- call(method_name, g2_likelihood$name, obs_data_call)
    if (length(g2_likelihood$fleetnames) > 0) out['fleets'] <- list(to_list_call(g2_likelihood$fleetnames))
    out['stocks'] <- list(to_list_call(g2_likelihood$stocknames))
    out['function_f'] <- list(function_f)
    out['report'] <- TRUE
    out['nll_breakdown'] <- TRUE
    out['weight'] <- as.double(g2_likelihood$weight)
    return(out)
}
