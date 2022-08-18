# Generate parameters suitable for R model
g2to3_params_r <- function(path, file_name, model_var_name = 'model_fn', out_var_name = file_name) {
    param_table <- utils::read.table(paste0(path, '/', file_name), header = TRUE, comment.char = ";")

    call("{", as.call(c(as.symbol("{"),  # }} NB: Double-nested { for neater g2to3_script output
        substitute(out_var <- attr(model_var, 'parameter_template'), list(
            model_var = as.symbol(model_var_name),
            out_var = as.symbol(out_var_name))),
        lapply(seq_len(nrow(param_table)), function (i) {
            substitute(out_var[[switch]] <- value, list(
                switch = param_table[i, 'switch'],
                value = param_table[i, 'value'],
                out_var = as.symbol(out_var_name)))
        }))))
}

g2to3_params_tmb <- function(path, file_name, model_var_name = 'model_cpp', out_var_name = file_name) {
    param_table <- utils::read.table(paste0(path, '/', file_name), header = TRUE, comment.char = ";")

    call("{", as.call(c(as.symbol("{"),  # }} NB: Double-nested { for neater g2to3_script output
        substitute(out_var <- attr(model_var, 'parameter_template'), list(
            model_var = as.symbol(model_var_name),
            out_var = as.symbol(out_var_name))),
        lapply(seq_len(nrow(param_table)), function (i) {
            substitute(out_var[switch, c('value', 'lower', 'upper', 'optimise')] <- c(value, lower, upper, optimise), list(
                switch = param_table[i, 'switch'],
                value = param_table[i, 'value'],
                lower = param_table[i, 'lower'],
                upper = param_table[i, 'upper'],
                optimise = as.logical(param_table[i, 'optimise']),
                out_var = as.symbol(out_var_name)))
        }))))
}
