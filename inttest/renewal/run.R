library(gadget3)
library(gadget2to3)
library(magrittr)
library(unittest)

year_range <- 1982:1990

lingimm <- g3_stock('lingimm', seq(20, 156, 4)) %>% 
    g3s_livesonareas(c("1" = 1)) %>%
    g3s_age(3, 5)

igfs <- g3_fleet('igfs') %>% g3s_livesonareas(c("1" = 1))

lingimm_actions <- list(
    g3a_initialconditions_normalparam(lingimm,
        factor_f = ~g3_param("lingimm.init") * g3_param("lingimm.init.scalar"),
        mean_f = ~g3_param("ling.Linf"),
        stddev_f = ~10, 
        alpha_f = ~g3_param("lingimm.walpha"),
        beta_f = ~g3_param("lingimm.wbeta")),
    g3a_renewal_normalparam(lingimm,
        factor_f = ~g3_param("lingimm.rec.scalar"),
        mean_f = ~g3_param("ling.Linf"),
        stddev_f = ~10, 
        alpha_f = ~g3_param("lingimm.walpha"),
        beta_f = ~g3_param("lingimm.wbeta"),
        run_f = ~cur_step == 1L&& area == 1L && age == 3L),
    g3a_naturalmortality(lingimm, g3a_naturalmortality_exp(~g3_param("lingimm.M"))),
    g3a_growmature(lingimm,
        impl_f = g3a_grow_impl_bbinom(
            g3a_grow_lengthvbsimple(~g3_param("ling.Linf"), ~0.001 * g3_param("ling.k")),
            g3a_grow_weightsimple(~g3_param("lingimm.walpha"), ~g3_param("lingimm.wbeta")),
            beta_f = ~10 * g3_param("ling.bbin"),
            maxlengthgroupgrowth = 15)),
    g3a_age(lingimm),
    list())

fleet_actions <- list(
    g3a_predate_fleet(igfs, list(lingimm),
        suitabilities = list(
            lingimm = g3_suitability_exponentiall50(~g3_param('ling.igfs.alpha'), ~g3_param('ling.igfs.l50'))),
        catchability_f = g3a_predate_catchability_totalfleet(g3_timeareadata('igfs_landings', Rgadget::read.gadget.file('inttest/renewal/', 'Data/fleet.igfs.data')[[1]], 'number'))),
    list())

ling_likelihood_actions <- list(
    g3l_understocking(
        weight = 100,
        list(lingimm)),
    g3l_catchdistribution(
        'ldist.igfs',
        weight = 1,
        obs_data = structure(
            Rgadget::read.gadget.file('inttest/renewal/', 'Data/catchdistribution.ldist.igfs.sumofsquares')[[1]],
            age = list(all3 = 3:5),
            length = Rgadget::read.gadget.file('inttest/renewal','Aggfiles/catchdistribution.ldist.igfs.len.agg')[[1]]),
        fleets = list(igfs),
        stocks = list(lingimm),
        g3l_distribution_sumofsquares()),
    list())

time_actions <- list(
    g3a_time(min(year_range), max(year_range), c(3,3,3,3)),
    list())

actions <- c(
  lingimm_actions,
#  fleet_actions,
#  ling_likelihood_actions,
  time_actions)
actions <- c(actions, list(g3a_report_history(actions)))

model_fn <- g3_to_r(actions, strict = FALSE, trace = FALSE)

actions <- local({
    eval(g2to3_mainfile('inttest/renewal'))
    c(actions, list(g3a_report_history(actions)))
})
model_fn_auto <- g3_to_r(actions, strict = FALSE, trace = FALSE)
ok(ut_cmp_identical(deparse(body(model_fn_auto), width.cutoff = 500L), deparse(body(model_fn), width.cutoff = 500L)), "g2to3 model matched hand-crafted model")
