g2to3_areafile <- function (path, file_name) {
    g2_area <- Rgadget::read.gadget.file(path, file_name, 'area', recursive = FALSE)

    area_names <- as.call(c( as.symbol("list"), as.list(as.character(g2_area[[1]]$areas)) ))
    
    return(substitute({
        area_names <- structure(seq_len(area_len), names = area_name_vals)
        area_size <- structure(area_size_vals, names = area_name_vals)
        area_temperature <- g2to3_data(path, file_name, 'area', 1, 'temperature')
    }, list(
        area_name_vals = area_names,
        area_len = length(area_names) - 1,
        area_size_vals = g2_area[[1]]$size,
        path = path,
        file_name = file_name)))
}
