# Read data.frame from end of gadget2 file
g2to3_data <- function(path, file_name, file_type = 'data', component = 1, sub_component = NULL) {
    x <- Rgadget::read.gadget.file(path, file_name, file_type)[[component]]
    if (!is.null(sub_component)) {
        x <- x[[sub_component]]
    }
    return(x)
}

# Read g2 data, convert to timeareadata
g2to3_timeareadata <- function (path, file_name, areas = NULL) {
    data <- Rgadget::read.gadget.file(path, file_name, 'data')[[1]]
    lookup_name <- gsub("\\W", ".", file_name, perl = TRUE)
    value_field <- tail(names(data), 1)
    gadget3::g3_timeareadata(lookup_name, data, value_field, areas = areas)
}

# Read data + aggregates
g2to3_aggdata <- function(
        path,
        file_name,
        areaaggfile = NULL,
        ageaggfile = NULL,
        lenaggfile = NULL,
        final_colname = NULL) {
    data <- Rgadget::read.gadget.file(path, file_name, 'data')[[1]]

    if (!is.null(areaaggfile)) {
        attr(data, 'area') <- as.list(Rgadget::read.gadget.file(path, areaaggfile)[[1]])
    }
    if (!is.null(ageaggfile)) {
        attr(data, 'age') <- as.list(Rgadget::read.gadget.file(path, ageaggfile)[[1]])
    }
    if (!is.null(lenaggfile)) {
        attr(data, 'length') <- as.list(Rgadget::read.gadget.file(path, lenaggfile)[[1]])
    }

    if (!is.null(final_colname)) {
        names(data)[length(names(data))] <- final_colname
    }

    return(data)
}

# Pre-extract all data in calls within (action_code)
extract_datacalls <- function(action_code, script_con, output_path) {
    # Write data to filesystem, and code to re-read it
    write_data <- function(lookup_name, data) {
        saveRDS(data, file = paste0(output_path, '/', lookup_name, '.rds'))
        cat(lookup_name, " <- readRDS('", lookup_name ,".rds')\n", sep = "", file = script_con)
    }
    call_replace(action_code,
        g2to3_data = function (fn, ...) {
            lookup_name <- gsub('\\W', "_", ..2, perl = TRUE)
            data <- do.call('g2to3_data', list(...))
            write_data(lookup_name, data)

            # Replace with reference to lookup_name
            as.symbol(lookup_name)
        }, g2to3_timeareadata = function(fn, ...) {
            x <- sys.call()  # NB: Can't use ..., since it contains a now-broken reference to area_names
            lookup_name <- gsub('\\W', "_", x[[4]], perl = TRUE)
            data <- Rgadget::read.gadget.file(x[[3]], x[[4]], 'data')[[1]]
            write_data(lookup_name, data)

            value_field <- tail(names(data), 1)
            call("g3_timeareadata", lookup_name, as.symbol(lookup_name), value_field, areas = x[['areas']])
        }, g2to3_aggdata = function (fn, ...) {
            lookup_name <- gsub('\\W', "_", ..2, perl = TRUE)
            data <- do.call('g2to3_aggdata', list(...))
            write_data(lookup_name, data)

            # Replace with reference to lookup_name
            as.symbol(lookup_name)
        })
}
