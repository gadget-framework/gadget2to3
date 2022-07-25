# Read data.frame from end of gadget2 file
g2to3_data <- function(path, file_name, file_type = 'data', component = 1, sub_component = NULL) {
    x <- Rgadget::read.gadget.file(path, file_name, file_type)[[component]]
    if (!is.null(sub_component)) {
        x <- x[[sub_component]]
    }
    return(x)
}

# Read g2 data, convert to timeareadata
g2to3_timeareadata <- function (path, file_name, areas = NULL, value_field = "number") {
    data <- Rgadget::read.gadget.file(path, file_name, 'data')[[1]]
    lookup_name <- gsub("\\W", ".", file_name, perl = TRUE)
    gadget3::g3_timeareadata(lookup_name, data, value_field, areas = areas)
}

# Read data + aggregates
g2to3_aggdata <- function(
        path,
        file_name,
        areaaggfile = NULL,
        ageaggfile = NULL,
        lenaggfile = NULL) {
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
    return(data)
}
