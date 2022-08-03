# Extract stock_vars list from suitability
sutabilities_stock_vars <- function(suitability) {
    as.call(c(as.symbol('list'), lapply(names(suitability), as.symbol)))
}

# Convert suitability into a g3 function list
sutabilities_fn_list <- function(path, suitability) {
    as.call(c(as.symbol("list"), lapply(suitability, function (suit) {
        if (suit[[1]] != 'function') stop("Invalid suitability ", suit)

        as.call(c(
            as.symbol(paste0("g3_suitability_", suit[[2]])),
            lapply(tail(suit, 2),  function(x) g2to3_formula(path, x))))
    })))
}
