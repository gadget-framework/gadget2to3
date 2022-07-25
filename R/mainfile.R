g2to3_mainfile <- function (path, file_name = 'main') {
    # Collapse double-bracing from e.g. multiple likelihood files
    collapse <- function (calls) {
        # Turn calls from list -> brace -> brace to list -> list -> brace
        calls <- lapply(calls, function (brace) tail(as.list(brace), -1))
        # Collapse outer list
        calls <- do.call("c", calls)
    }

    g2_main <- Rgadget::read.gadget.file(path, file_name, 'main', recursive = FALSE)
    out <- as.call(c(as.symbol("{"),  # }
        list(g2to3_timefile(path, g2_main[[1]]$timefile)),
        list(g2to3_areafile(path, g2_main[[1]]$areafile)),

        lapply(g2_main$stock$stockfiles, function (f) g2to3_stockfile(path, f)[[2]]),
        lapply(g2_main$stock$stockfiles, function (f) g2to3_stockfile(path, f)[[3]]),

        collapse(lapply(g2_main$fleet$fleetfiles, function (f) g2to3_fleetfile(path, f))),

        collapse(lapply(g2_main$likelihood$likelihoodfiles, function (f) g2to3_likelihoodfile(path, f))),

        list(quote(
            actions <- combined_actions
        ))))

    # Replace combined_actions with concatenated actions
    out <- call_replace(out, combined_actions = function (sym) {
        # Get all action vars, generate code to combine the lists
        action_vars <- grep('^actions_', all.vars(out), value = TRUE)
        as.call(lapply(c("c", action_vars), as.symbol))
    })
    out
}
