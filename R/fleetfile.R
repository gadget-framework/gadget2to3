g2to3_fleetfile <- function (path, file_name) {
    g2_fleets <- Rgadget::read.gadget.file(path, file_name, 'fleet', recursive = FALSE)
    
    as.call(c(as.symbol("{"),  # }
        lapply(g2_fleets, function (x) fleet(path, x))))
}

fleet <- function (path, g2_fleet) {
    fleet_var <- as.symbol(g2_fleet[[1]])

    # Fetch fleet's predation action
    action <- get_g2tog3("fleet_", names(g2_fleet)[[1]])(path, fleet_var, g2_fleet)

    substitute({
        comment(comment_str)
        fleet_var <- g3_fleet(fleetname)
        fleet_var <- g3s_livesonareas(fleet_var, livesonareas)
        actions_fleet_var <- list(action)
    }, c(list(
        comment_str = paste0("Create fleet definition for ", g2_fleet[[1]]),
        fleetname = g2_fleet[[1]],
        livesonareas = substitute(area_names[x], list(x = as.character(g2_fleet[[1]]$livesonareas))),
        actions_fleet_var = as.symbol(paste0('actions_', fleet_var)),
        action = action,
        fleet_var = fleet_var), g2_fleet))
}

fleet_totalfleet <- function (path, fleet_var, g2_fleet) {
   substitute(
       g3a_predate_fleet(fleet_var, stock_vars,
           suitabilities = sutabilities,
           catchability_f = g3a_predate_catchability_totalfleet(
               g2to3_timeareadata(path, data_file_name, areas = area_names))), list(
       fleet_var = fleet_var,
       stock_vars = sutabilities_stock_vars(g2_fleet$suitability),
       sutabilities = sutabilities_fn_list(g2_fleet$suitability),
       path = path,
       data_file_name = g2_fleet$amount))
}
