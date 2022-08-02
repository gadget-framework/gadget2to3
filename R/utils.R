list.all.equal <- function (l) {
    for (i in seq_along(l)) {
        out <- if (i == 1) TRUE else all.equal(l[[1]], l[[i]])
        if (!isTRUE(out)) return(FALSE)
    }
    return(TRUE)
}

# Dynamically find something in our namespace
get_g2tog3 <- function (...) {
    fn_name <- paste0(...)
    if (exists(fn_name,  envir = getNamespace("gadget2to3"))) {
        get(fn_name, envir = getNamespace("gadget2to3"))
    } else {
        # Return code producing a non-functioning model
        function(...) call("stop", paste0(fn_name, " is not supported"))
    }
}

# Combine table of forumlas, symbol and value into an if statement
combine_formulas <- function (fs, default = quote(stop()), ...) {
    stopifnot(is.list(fs))
    for (i in seq_len(...length())) stopifnot(length(...elt(i)) == length(fs))
    vals <- list(...)

    # All the same, doesn't make a difference what we choose
    if (list.all.equal(fs)) return(fs[[1]])

    # Build if call for each value, using temporary symbols for formulas
    names(fs) <- paste0("__tmp_combine_formulas_", seq_along(fs))
    out <- Reduce(
        function (i, rest) {
            # Compare current value for all values given
            out <- lapply(names(vals), function(n) call("==", as.symbol(n), vals[[n]][[i]]))
            # Combine with && calls
            out <- gadget3:::f_optimize(Reduce(function (f, rest) call("&&", rest, f), out, TRUE))
            # Add to an if statement
            call("if", out, as.symbol(names(fs)[[i]]), rest)
        },
        seq_along(fs),
        default,
        right = TRUE)

    # Replace temporary symbols with actual formulas
    gadget3:::f_substitute(out, fs)
}
# combine_formulas(list(~a, ~b), age = c(1,2), area = c(4,5))

# Descend through call f, when a symbol like key appears, call it's function to modify the call
call_replace <- function (f, ...) {
    # Either take a single list or use the remaining argument list
    modify_fns <- if (...length() == 1 && is.list(..1)) ..1 else list(...)
    # Split function list based in first parameter
    modify_fns <- list(
        sym = Filter(function (fn) { names(formals(fn))[[1]] == 'sym' }, modify_fns),
        int = Filter(function (fn) { names(formals(fn))[[1]] == 'int' }, modify_fns),
        num = Filter(function (fn) { names(formals(fn))[[1]] == 'num' }, modify_fns),
        fn = Filter(function (fn) { names(formals(fn))[[1]] == 'fn' }, modify_fns))

    call_replace_inner <- function(f) {
        if (is.symbol(f)) {
            modify_fn <- modify_fns$sym[[as.character(f)]]
            if (length(modify_fn) > 0) f <- modify_fn(f)
            return(f)
        }

        if (is.integer(f) && length(f) == 1) {  # TODO: need to support vectors here
            modify_fn <- modify_fns$int[[as.character(f)]]
            if (length(modify_fn) > 0) f <- modify_fn(f)
            return(f)
        }

        if (is.numeric(f) && length(f) == 1) {  # TODO: need to support vectors here
            modify_fn <- modify_fns$num[[as.character(f)]]
            if (length(modify_fn) > 0) f <- modify_fn(f)
            return(f)
        }

        if (is.call(f)) {
            # If there's a modify_fn that matches the symbol of this call, call it.
            # NB: Use deparse() to generate useful output for, e.g. Matrix::Matrix
            modify_fn <- modify_fns$fn[[deparse(f[[1]])]]
            if (length(modify_fn) > 0) f <- do.call(modify_fn, as.list(f))

            # Recurse through all arguments of this call (but leave the calling symbol alone)
            out <- as.call(c(
                f[[1]],
                lapply(tail(f, -1), call_replace_inner)))

            # Put back all attributes (i.e. keep formula-ness)
            attributes(out) <- attributes(f)
            return(out)
        }

        return(f)
    }

    # Start recursing
    call_replace_inner(f)
}
