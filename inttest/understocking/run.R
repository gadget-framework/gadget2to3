library(gadget3)
library(gadget2to3)
library(magrittr)
library(unittest)

year_range <- 1982:1990

lingimm <- g3_stock('lingimm', seq(20, 156, 4)) %>% 
    g3s_livesonareas(c("1" = 1)) %>%
    g3s_age(3, 5)

lingmat <- g3_stock('lingmat', seq(20, 156, 4)) %>%
    g3s_livesonareas(c("1" = 1)) %>%
    g3s_age(5, 15)

igfs <- g3_fleet('igfs') %>% g3s_livesonareas(c("1" = 1))

nll_report <- rep(0, length(year_range) * 4)
prev_nll <- 0.0
remove_nll_attributes <- gadget3:::g3_native(r = function (x) x[[1]], cpp = "[](Type x) -> Type { return x; }")

lingimm_actions <- list(
    g3a_initialconditions_normalparam(lingimm,
        factor_f = ~(age * g3_param("lingimm.init")) * g3_param("lingimm.init.scalar"),
        mean_f = ~g3_param("ling.Linf"),
        stddev_f = ~10, 
        alpha_f = ~g3_param("lingimm.walpha"),
        beta_f = ~g3_param("lingimm.wbeta")),
    g3a_naturalmortality(lingimm, g3a_naturalmortality_exp(~g3_param("lingimm.M"))),
    g3a_age(lingimm),
    list())

lingmat_actions <- list(
    g3a_initialconditions_normalparam(lingmat,
        factor_f = ~(0.2 * (age * g3_param("lingmat.init"))) * g3_param("lingmat.init.scalar"),
        mean_f = ~g3_param("ling.Linf"),
        stddev_f = ~10,
        alpha_f = ~g3_param("lingmat.walpha"),
        beta_f = ~g3_param("lingmat.wbeta")),
    g3a_naturalmortality(lingmat, g3a_naturalmortality_exp(~g3_param("lingmat.M"))),
    g3a_age(lingmat),
    list())

fleet_actions <- list(
    g3a_predate_fleet(igfs, list(lingimm, lingmat),
        suitabilities = list(
            lingimm = g3_suitability_exponentiall50(~g3_param('ling.igfs.alpha'), ~g3_param('ling.igfs.l50')),
            lingmat = g3_suitability_exponentiall50(~g3_param('ling.igfs.alpha'), ~g3_param('ling.igfs.l50'))),
        catchability_f = g3a_predate_catchability_totalfleet(g3_timeareadata('Data.fleet.igfs.data', Rgadget::read.gadget.file('inttest/understocking/', 'Data/fleet.igfs.data')[[1]], 'number'))),
    list())

ling_likelihood_actions <- list(
    g3l_understocking(
        weight = 10,
        nll_breakdown = TRUE,
        list(lingimm, lingmat)),
    g3l_catchdistribution(
        'ldist.igfs.ss',
        weight = 10,
        obs_data = structure(
            Rgadget::read.gadget.file('inttest/understocking/', 'Data/catchdistribution.ldist.igfs.sumofsquares')[[1]],
            age = Rgadget::read.gadget.file('inttest/understocking','Aggfiles/catchdistribution.ldist.igfs.age.agg')[[1]],
            length = Rgadget::read.gadget.file('inttest/understocking','Aggfiles/catchdistribution.ldist.igfs.len.agg')[[1]]),
        fleets = list(igfs),
        stocks = list(lingimm, lingmat),
        g3l_distribution_sumofsquares(),
        report = TRUE,
        nll_breakdown = TRUE),
    g3l_catchdistribution(
        'ldist.igfs.mn',
        weight = 10,
        obs_data = structure(
            Rgadget::read.gadget.file('inttest/understocking/', 'Data/catchdistribution.ldist.igfs.sumofsquares')[[1]],
            age = Rgadget::read.gadget.file('inttest/understocking','Aggfiles/catchdistribution.ldist.igfs.age.agg')[[1]],
            length = Rgadget::read.gadget.file('inttest/understocking','Aggfiles/catchdistribution.ldist.igfs.len.agg')[[1]]),
        fleets = list(igfs),
        stocks = list(lingimm, lingmat),
        g3l_distribution_multinomial(),
        report = TRUE,
        nll_breakdown = TRUE),
    g3l_abundancedistribution(
        'si.100-120',
        weight = 40,
        obs_data = structure(
            Rgadget::read.gadget.file('inttest/understocking/', 'Data/surveyindices.si.100-120.lengths')[[1]],
            length = Rgadget::read.gadget.file('inttest/understocking','Aggfiles/surveyindices.si.100-120.len.agg')[[1]]),
        fleets = list(),
        stocks = list(lingimm, lingmat),
        g3l_distribution_surveyindices_log(alpha = NULL, beta = NULL),
        report = TRUE,
        nll_breakdown = TRUE),
    list())

report_actions <- list(
       list('999' = ~{
           nll_report[[cur_time + 1]] <- nll - prev_nll
           prev_nll <- remove_nll_attributes(nll)
       }))

time_actions <- list(
    g3a_time(min(year_range), max(year_range), c(3,3,3,3)),
    list())

actions <- c(
  lingimm_actions,
  lingmat_actions,
  fleet_actions,
  ling_likelihood_actions,
  report_actions,
  time_actions)
actions <- c(actions, list(g3a_report_history(actions, var_re = "__num$|__wgt$|__consratio$|__totalpredate$|__igfs$|__catch$")))

model_fn <- g3_to_r(actions, strict = TRUE, trace = FALSE)

actions <- local({
    eval(g2to3_mainfile('inttest/understocking'))
    c(actions, report_actions, list(g3a_report_history(actions, var_re = "__num$|__wgt$|__consratio$|__totalpredate$|__igfs$|__catch$")))
})
model_fn_auto <- g3_to_r(actions, strict = TRUE, trace = FALSE)
ok(ut_cmp_identical(deparse(body(model_fn_auto), width.cutoff = 500L), deparse(body(model_fn), width.cutoff = 500L)), "g2to3 model matched hand-crafted model")
